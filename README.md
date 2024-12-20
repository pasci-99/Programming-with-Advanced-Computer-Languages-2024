# Programming-with-Advanced-Computer-Languages-2024
Hedonic Housing Price Regression and Spatial Data Analysis

Author: Pascal Simon (19-615-731) (Pasci on CodingXCamp)

Platform: R executed in RStudio

Dataset: 600K US Housing Properties.csv from https://www.kaggle.com/datasets/polartech/500000-us-homes-data-for-sale-properties

---



**Project Overview**

This repository contains an R programming project for the course Skills: Programming with Advanced Computer Languages. The project investigates housing prices through hedonic regression models and spatial data analysis. It also features:
- Exploratory Data Analysis to understand trends and relationships.
- Regression Modeling to understand and predictive housing prices.
- Interactive Visualizations and Maps to display spatial information and patterns.
- Shiny Dashboard for user-friendly, interactive prediction of housing prices.

---

**Outputs**

The project generates the following outputs:
1) Regression Results:
   - Summary statistics
   - Comparative analyses
   - Model diagnostics
2) Visualizations:
   - Multiple Interactive maps showcasing the observation with additional added spatial information
   - Diagnostic plots
3) Shiny Dashboard:
	•	Interactive tool for estimating housing prices for ones dream home in the selected cities.

---

**Instructions**

The R code should be executed step by step to reproduce the intended results. Note that certain requests to OpenStreetMap and some calculations may take time to execute, so please be patient.

Follow the steps below to set up and execute the project. All necessary instructions are provided within the code itself and can be followed directly. It is recommended to complete steps 1 and 2 first, then proceed with the instructions in the code:


  1.	Clone or Download this repository to your local system.
  2.	Open the Script: HedonicHousingPriceRegressionAndSpatialDataProject.R ideally in RStudio.
  3.	Download the Dataset:
      - Obtain the dataset “600K US Housing Properties.csv” from Kaggle. https://www.kaggle.com/datasets/polartech/500000-us-homes-data-for-sale-          properties
      - Save the dataset as 600K US Housing Properties.csv in a designated folder.
  4. Set Up the Environment:
      - Copy the path to the folder containing the dataset and paste it into the appropriate location in the R script to set the working directory.
  5. Install Required Packages:
      - Run the provided commands in the script to install and load necessary R packages.
  6. Execute the Script:
      - Follow the instructions embedded in the R script to perform the analysis step by step.
      - Some Outputs (plots, diagnostics, interactive maps, etc.) will be saved to the same folder as the dataset.

---

Much of the code was written using functions, such that the cities analyzed can be changed with a reasonable effort.
However making everything generally applicable for all cities is impossible, since different cleaning steps are needed for different cities.

**Have fun with the Analysis! :)**
