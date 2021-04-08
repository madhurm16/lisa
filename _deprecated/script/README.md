# R Script list

These scripts are used to generate data, to treat data, to calibrate parameters and to produce graphics.

## Description of all used scripts :

- **data_initialization.R** : convert raw data into .csv format.

- **generate_demography.R** : generate demographic variables for simulation from the UN data and populstat dataset.

- **graph_babyboomersFRgif.R** : generate graphs of the french population distribution for each year and produce a .gif file.
- **graph_counterfactual.R** : generate graphs for the counterfactual simulations.
- **graph_decomposition.R** : generate graphs for the decomposition of the direct and indirect cohort-effects.
- **graph_demo.R** : generate a graph for the demographic variables (n, p and the dependency ratio) used in simulation.

- **sim_AFinder.R** : grid search to calibrate the scale parameter (A).

- **treatment_cwed.R** : treat the dataset from the CWED Dataset.
- **treatment_pwt.R** : treat and merge all datasets from the Penn World Table.
- **treatment_oecd.R** : treat and merge all datasets from the OECD Database.
- **treatment_un.R** : treat the dataset from the United Nations.
