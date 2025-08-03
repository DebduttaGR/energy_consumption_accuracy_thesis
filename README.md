# Two-Phase ML Energy Evaluation

This repository contains all code and assets required to reproduce the experiments for our study **"On the Energy Consumption and Accuracy of ML Model Composition: A Controlled Experiment With Two-Stage Prediction Use Cases."**

We explore two real-world machine learning pipelines—Flight Delay Prediction and ICU Mortality/Time-to-Death Prediction—and evaluate energy, latency, and accuracy trade-offs using a confidence-based gating mechanism. The experiments rely on two machines:

* One **orchestrator** device that controls the experiment remotely via SSH.
* One **target** device where the actual ML models run and energy is measured.

---

## ⚙️ Setup Instructions

### 1. Clone the Repository

Clone this repository on **both** the orchestrator and the target devices:

```bash
git clone https://github.com/your-username/two-phase-ml-energy.git
cd two-phase-ml-energy
```

---

### 2. Install Dependencies

You can either:

* Follow the `README.md` inside each folder to install dependencies specific to that module, **or**
* Install all dependencies using the global `requirements.txt`:

```bash
pip install -r requirements.txt
```

Ensure you are using **Python 3.11** on the target device for best compatibility.

---

### 3. Install EnergiBridge (Target Device Only)

EnergiBridge is required to measure energy consumption. On the **target device**:

* Navigate to the `EnergiBridge/` directory and follow the setup instructions there.
* Make sure EnergiBridge is properly installed and configured before running experiments.

---

## ✈️ Flight Delay Prediction Use Case

### Step 1: Train the Models

Run the following notebooks on the **target device**, in this order:

1. `FlightDelayPrediction/MLProjectRandomForestClassifieripynb.ipynb`
2. `FlightDelayPrediction/MLProjectRandomForestPipeliningAndRegressionAnalysis.ipynb`
3. `FlightDelayPrediction/MLProjectRandomForestRegressor.ipynb`

These notebooks will generate the necessary `.pkl` model files.

---

### Step 2: Run the Experiment (from Orchestrator)

From the **orchestrator device**, use the **experiment-runner** to run the experiment remotely via SSH.

Use the following Python script:

```
experiment-runner/examples/uc1/RunnerConfig.py
```

Follow the instructions in the `experiment-runner/README.md` to understand how to run Python scripts on remote devices.

---

## 🏥 Predicting Death Time and Mortality Use Case

### Step 1: Set Up the PostgreSQL Database

On the **target device**:

* Set up a PostgreSQL database with the required tables, based on the referenced research paper.
* Update the credentials and connection details in the `connect_db()` function inside:

```
Predicting-Death-Time-and-Mortality/Model/Code/Notebooks/utils.py
```

* Run:

```bash
Predicting-Death-Time-and-Mortality/Model/Code/Notebooks/test_db.py
```

to verify that data is being correctly fetched.

---

### Step 2: Train the Models

Run the following notebooks on the **target device**:

1. `Predicting-Death-Time-and-Mortality/Model/Code/Notebooks/Phase1_model.ipynb`
2. `Predicting-Death-Time-and-Mortality/Model/Code/Notebooks/Phase2_model.ipynb`

These notebooks will generate the trained models for inference.

---

### Step 3: Run the Experiment (from Orchestrator)

From the **orchestrator device**, use **experiment-runner** to execute the experiment remotely:

Use the following script:

```
experiment-runner/examples/uc2/RunnerConfig.py
```

Follow the `experiment-runner` documentation for remote execution instructions.

---

## 📊 Analysis

To analyze and visualize the results:

* Open the `analysis/descriptive_analysis.R` file in **RStudio**.
* Run the script to generate statistical plots and summaries.

---

## 📁 Repository Structure (Overview)

```
.
├── FlightDelayPrediction/
├── Predicting-Death-Time-and-Mortality/
├── EnergiBridge/
├── experiment-runner/
├── analysis/
└── README.md
```

---

## 📬 Contact & Citation

For questions, feedback, or contributions, please open an issue or reach out via \[d.guha.roy@student.vu.nl].

If you use this repository in your research, please consider citing the associated paper

> Debdutta Guha Roy, *On the Energy Consumption and Accuracy of ML Model Composition: A Controlled Experiment With Two-Stage Prediction Use Cases*, 2025.
