# Parasitic-Diptera-Bee
This repository contains R scripts and bee phylogenetic tree used to perform analyses regarding the effects of bee natural history traits on Diptera parasitism rate

---

## Associated publication (currently under review)

**Title:** *Sociality and nesting strategy account for non-random associations between bees and their parasitic dipterans* - 
**Authors:** *Carlo Polidori, Andrea Ferrari* - 
**Journal:** *Insectes Sociaux* - 
**Year:** *2026* - 
**DOI:** *Currently under review* - 

---

## Necessary files to run the statistical analyses

* `Script_Diptera-bees` → Main script for the full analysis pipeline
* `DIPTBEE.xlsx` → Datafile (available at publisher page as supplementary file)
* `Tree_Diptera-Bees` → Phylogenetic tree

When the script is run, the following folders will appear:

  * `results/` → Statistical outputs (e.g. models, tables)
  * `figures/` → Maps and graphical outputs

---

## Workflow overview

1. Clone the **necessary files**
2. Open and run `Alien-Bees.R` in RStudio

The analysis pipeline is divided into modular sections:

## Script structure

* **0 – Prepare the environment** → Installs and loads required R packages, creates output directories, checks the working directory, and prepares the R environment.

* **1 – Network** → Builds Diptera–bee interaction matrices, generates bipartite networks, calculates modularity and specialization metrics, and exports summary tables.

* **2 – Models** → Runs phylogenetic linear and logistic models testing the effects of bee traits and climate on parasitism richness, strategies, and specialization.

* **3 – Plots** → Produces publication-ready figures including barplots, spiderplots, specialization plots, and ancestral state reconstructions.

---


## Requirements

The script automatically installs missing packages. R 4.4+ is highly recommended

---


## Contact

For questions or data requests: *a.ferrari.research@gmail.com*

---


## Citation

Please cite: *Currently under review* when using the script or the data
