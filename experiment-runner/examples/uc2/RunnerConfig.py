# RunnerConfig.py
import os
import subprocess
import time
import pandas as pd
import shutil

from subprocess import CalledProcessError
from pathlib import Path
from typing import Dict, Any
from EventManager.Models.RunnerEvents           import RunnerEvents
from EventManager.EventSubscriptionController   import EventSubscriptionController
from ConfigValidator.Config.Models.RunTableModel import RunTableModel
from ConfigValidator.Config.Models.FactorModel   import FactorModel
from ConfigValidator.Config.Models.RunnerContext import RunnerContext
from ConfigValidator.Config.Models.OperationType import OperationType
from ProgressManager.Output.OutputProcedure     import OutputProcedure as output
from Plugins.Profilers.EnergiBridge             import EnergiBridge

class RunnerConfig:
    ROOT_DIR           = Path(__file__).parent

    # ————— USER CONFIG —————
    name                = "predicting_death_time_and_mortality_ssh"
    results_output_path = ROOT_DIR / "experiments"
    operation_type      = OperationType.AUTO
    time_between_runs_in_ms = 60000

    # SSH / remote settings
    ssh_host           = "GL5"
    ssh_user           = "debdutta"
    remote_workdir     = "/home/debdutta/Experiment/Predicting-Death-Time-and-Mortality/Model/Code/Notebooks"
    remote_activate    = "source /home/debdutta/Experiment/uc2/bin/activate"

    def __init__(self):
        EventSubscriptionController.subscribe_to_multiple_events([
            (RunnerEvents.BEFORE_EXPERIMENT, self.before_experiment),
            (RunnerEvents.BEFORE_RUN,        self.before_run),
            (RunnerEvents.START_MEASUREMENT, self.start_measurement),
            (RunnerEvents.STOP_MEASUREMENT,  self.stop_measurement),
            (RunnerEvents.STOP_RUN,          self.stop_run),
            (RunnerEvents.POPULATE_RUN_DATA, self.populate_run_data),
            (RunnerEvents.AFTER_EXPERIMENT,  self.after_experiment),
        ])
        self.profiler = None
        self.ml_metrics: Dict[str, float] = {}
        output.console_log("✔ RunnerConfig loaded (remote Energibridge)")

    def create_run_table_model(self) -> RunTableModel:
        f_uc = FactorModel("usecase",   ["PDTM"])
        f_th = FactorModel("threshold", [0.1,0.3,0.5,0.7,0.9,1.0])
        reps = 1 if os.getenv("TEST_MODE")=="1" else 20

        self.run_table_model = RunTableModel(
            factors     = [f_uc, f_th],
            repetitions = reps,
            data_columns=[
                "energy_j","runtime_s","cpu_w","mem_mb","invocation_rate",
                "roc_auc_6h","accuracy_6h","f1_macro_6h",
                "roc_auc_12h","accuracy_12h","f1_macro_12h",
                "roc_auc_24h","accuracy_24h","f1_macro_24h"
            ]
        )
        return self.run_table_model

    def before_experiment(self):
        output.console_log(">>> Starting Predicting Death Time and Mortality on remote server")

    def before_run(self):
        pass

    def start_measurement(self, context: RunnerContext):
        th      = context.execute_run["threshold"]
        ssh     = f"{self.ssh_user}@{self.ssh_host}"
        run_cmd = f"python3 two_phase_pipeline.py --threshold {th} > metrics.csv"
        eb_cmd  = "energibridge --output energibridge.csv --summary"

        remote_cmd = (
            f"{self.remote_activate} && "
            f"cd {self.remote_workdir} && "
            f"{eb_cmd} {run_cmd}"
        )
        ssh_cmd = f'ssh {ssh} "{remote_cmd}"'
        output.console_log(f"[DEBUG] SSH CMD: {ssh_cmd}")

        last_err = None
        for attempt in range(1, 4):
            proc = subprocess.Popen(
                ssh_cmd, shell=True, executable="/bin/bash",
                stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
            context._remote_proc = proc
            output.console_log(f"[DEBUG] remote PID on this machine: {proc.pid} (attempt {attempt})")

            rc = proc.wait()
            if rc == 0:
                output.console_log(f"[DEBUG] remote measurement succeeded on attempt {attempt}")
                return
            stderr = proc.stderr.read().decode().strip()
            last_err = stderr
            output.console_log(f"[WARN] measurement attempt {attempt} failed: {stderr}")
            time.sleep(2)

        # if we reach here, all retries failed
        raise RuntimeError(f"Remote measurement failed after 3 attempts:\n{last_err}")


    def stop_measurement(self, context: RunnerContext):
        # just in case something's still running
        proc = getattr(context, "_remote_proc", None)
        if proc and proc.poll() is None:
            proc.kill()

    def _fetch_remote_results(self, context: RunnerContext):
        # # 1) make sure the SSH job is done
        # proc = getattr(context, "_remote_proc", None)
        # if proc is not None:
        #     rc = proc.wait()
        #     if rc != 0:
        #         raise RuntimeError(f"remote pipeline exit code {rc}")

        # # 2) scp both CSVs back
        # ssh = f"{self.ssh_user}@{self.ssh_host}"
        # remote_base = self.remote_workdir.rstrip("/")
        # local_dir   = str(context.run_dir)
        # for fname in ("metrics.csv", "energibridge.csv"):
        #     remote = f"{ssh}:{remote_base}/{fname}"
        #     local  = Path(local_dir) / fname
        #     subprocess.check_call(["scp", remote, str(local)])
        # 1) make sure the remote job is done
        proc = getattr(context, "_remote_proc", None)
        if proc is not None:
            rc = proc.wait()
            if rc != 0:
                raise RuntimeError(f"remote pipeline exit code {rc}")

        # 2) pull both CSVs back, retrying on transient failures
        ssh         = f"{self.ssh_user}@{self.ssh_host}"
        remote_base = self.remote_workdir.rstrip("/")
        local_dir   = str(context.run_dir)
        for fname in ("metrics.csv", "energibridge.csv"):
            remote = f"{ssh}:{remote_base}/{fname}"
            local  = Path(local_dir) / fname
            last_exc = None
            for attempt in range(1, 4):
                try:
                    subprocess.check_call(["scp", remote, str(local)])
                    break
                except Exception as e:
                    last_exc = e
                    output.console_log(f"[WARN] scp attempt {attempt} for {fname} failed: {e}")
                    time.sleep(2)
            else:
                # after 3 attempts still failing
                raise RuntimeError(f"Could not fetch `{fname}` after 3 tries: {last_exc}")

    def stop_run(self, context: RunnerContext):
        # fetch the two result files
        self._fetch_remote_results(context)

        # parse metrics.csv
        rows = pd.read_csv(context.run_dir / "metrics.csv", header=0)
        rows = rows[rows['window'].apply(lambda x: str(x).replace('.','',1).isdigit())]
        self.ml_metrics = []
        for _, row in rows.iterrows():
            self.ml_metrics.append({
                "window_hours": float(row["window"]),
                "ROC_AUC":      float(row["roc_auc"]),
                "Accuracy":     float(row["accuracy"]),
                "F1-Macro":     float(row["macro_f1"]),
            })
    
    def populate_run_data(self, context: RunnerContext) -> Dict[str, Any]:
        # 1) resource metrics
        df = pd.read_csv(context.run_dir / "energibridge.csv")
        energy  = df["PACKAGE_ENERGY (J)"].iloc[-1] - df["PACKAGE_ENERGY (J)"].iloc[0]
        runtime = df["Time"].max() - df["Time"].min()
        cpu_cols = [c for c in df.columns if c.startswith("CPU_USAGE_")]
        cpu     = df[cpu_cols].mean().mean()
        mem     = df["USED_MEMORY"].mean()

        # 2) flatten self.ml_metrics into keyed values
        flat = {}
        for m in self.ml_metrics:
            w = int(m["window_hours"])       # 6, 12, or 24
            flat[f"roc_auc_{w}h"]   = m["ROC_AUC"]
            flat[f"accuracy_{w}h"]  = m["Accuracy"]
            flat[f"f1_macro_{w}h"]  = m["F1-Macro"]

        # 3) build and return a single dict
        return {
            "energy_j":       energy,
            "runtime_s":      runtime,
            "cpu_w":          cpu,
            "mem_mb":         mem,
            "invocation_rate": context.execute_run["threshold"],
            **flat
        }



    def after_experiment(self):
        output.console_log(">>> Completed all runs for Predicting Death Time and Mortality (SSH)")

    # ————— DO NOT ALTER BELOW —————
    experiment_path: Path = None
