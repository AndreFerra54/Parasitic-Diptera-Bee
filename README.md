# Parasitic-Diptera-Bee
This repository contains R scripts and bee phylogenetic tree used to perform analyses regarding the effects of bee natural history traits on Diptera parasitism rate

---

## Associated publication

**Title:** *[Insert full manuscript title]*
**Authors:** *[Insert authors]*
**Journal:** *[Insert journal name]*
**Year:** *[Insert year]*
**DOI:** *[Insert DOI]*

---

## Necessary files to run the statistical analyses

* `Alien-Bees.R` → Main script for the full analysis pipeline
* `Alien-bees.xlsx` → Metadata and model input data
* `Bee_Tree.nwk` → Phylogenetic tree
* `db_final.rds` → RDS file with presence records

When the script is run, the following folders will appear:
  * `rawdata/` → Raw and intermediate datasets (generated automatically)
  * `results/` → Statistical outputs (e.g. models, tables)
  * `figures/` → Maps and graphical outputs

---

## Workflow overview

1. Clone the **necessary files**
2. Open `Alien-Bees.R` in RStudio
3. Run:

   * Section **0** → mandatory setup
   * Sections **1–2** → ONLY for raw data dowload. Refined data are already present in `db_final.rds`
   * Sections **3–6** → full analysis


The analysis pipeline is divided into modular sections:

* **0 – Environment preparation** → Installs and loads required R packages, sets up directories

* **1 – Data download** → Retrieves occurrence records from GBIF and ALA

* **2 – Data processing** → Cleans records, assigns native/invasive status, extracts environmental variables

* **3 – Mapping** → Generates species distribution maps and global heatmaps

* **4 – Niche modelling** → Performs PCA and calculates niche overlap and dynamics metrics

* **5 – Statistical models** → Runs ordinar linear and phylogenetic models

* **6 – Plots** → Produces some of the figures used in the manuscript


**Note**: Data download requires GBIF and ALA credentials. Users must set GBIF credentials via *.Renviron*

---

## Requirements

The script automatically installs missing packages. R 4.4+ is highly recommended

---


## Contact

For questions or data requests: *a.ferrari.research@gmail.com*

---

## Citation

Please cite: *[Insert paper citation]* when using the script or the data
