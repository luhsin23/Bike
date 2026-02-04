# Bike Data & Code Archive

This repository contains the data, source code, and output files for the bike rental analysis project.

---

## 📂 Directory Structure

### `data/`
* `raw.csv`: The primary raw dataset containing transaction details.
* `bike.data.csv`: Pre-processed dataset derived from `raw.csv`, used for core analysis.

### `code/`
Contains **R** and **Jupyter Notebook** scripts for simulation, estimation, and visualization. Refer to the [Code Documentation](#-code-documentation) section for details.

### `output/`
* `estimation/`: Results from parameter estimation models.
* `CSL/`: Customer Service Level (CSL) tables.
* `plot/`: Visualizations including demand curves, histograms, and trace plots.

---

## 📊 Data Details: `raw.csv`
> **Description:** This file contains the raw transaction information used for the analysis.

* **Contents:** Each row represents a transaction including `rental station` and `total rental fee`.
* **Currency:** Fees are in **New Taiwan Dollar (NTD)**. 
    * *Note: 1 NTD ≈ $0.035 USD (as of Oct 12, 2020).*
* **Usage:** Descriptive statistics in **Table 4** and **Table 5** are derived directly from this file. It is also the source for the pre-processed `bike.data.csv`.

---

## 💻 Code Documentation

### 1. Simulation Scripts

| File | Description | Key Outputs |
| :--- | :--- | :--- |
| `simulation.R` | Simulates quantities for inside/outside goods (**Table 1**). | `s.x.1` (inside), `s.x.0` (outside) |
| `simulation_likelihood.R` | Produces likelihoods and estimates (**Table 2**). | Likelihood variables `TL.1-6` and `L.1-6` |

### 2. Estimation & Optimization

#### `estimation.R`
* **Purpose:** Runs the MCMC algorithm (Appendix 2) to produce **Table 6**.
* **Input:** `bike.data.csv`
* **Outputs (`output/estimation/`):** `mean_psi.csv`, `mean_lambda.csv`, `sd_psi.csv`, `sd_lambda.csv`, and full result sets.

#### `gurobi.ipynb`
* **Purpose:** Python implementation using `gurobipy` to calculate price elasticity (**Table 7**).
* **Procedure:** Solves the Mixed-Integer Program (MIP) defined in **Procedure 1**.
* **Outputs:** `df_elasticity` (mean and SD for 18 rental options).

#### `estimation_am.R`
* **Purpose:** Estimates parameters specifically for the **Gongguan Area** (**Table 8**).
* **Input:** Filtered `bike.data.csv` using `mean_psi.csv` as starting values.

---

### 3. Analysis & Visualizations

#### `CSL.R`
* **Description:** Computes Customer Service Levels (**Table 9**).
* **Outputs (`output/CSL/`):** Generates tables for discount-driven demand ranging from **20% to 80%** (e.g., `table.50.50.csv`).

#### `demand_curve.R`
* **Description:** Plots demand curves (**Figure 1**).
* **Outputs:** `.eps` files for rental options 1 through 6 in `output/plot/demand_curves/`.

#### `trace_plot.R` & `histgram.R`
* **Description:** Generates diagnostic plots for MCMC chains (**Appendix 3**).
* **Outputs:** * **Trace Plots:** Located in `output/plot/psi_trace/` and `lambda_trace/`.
    * **Histograms:** Located in `output/plot/psi_hist/` and `lambda_hist/`.
    * *Note: Covers all 18 rental options for both parameters.*
