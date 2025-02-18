# CSF.Ind: Climate Smart Forestry Indicators Generator
🌎⛰️🌳🌲 **CSF.Ind** is a user-friendly Shiny web application designed to calculate Climate Smart Forestry (CSF) indicators using proximal data (e.g., field surveys or remote forest inventory data). This tool is tailored for European forests and supports decision-making in forest management under Sustainable Forest Management (SFM) criteria.

![Image](https://github.com/user-attachments/assets/df9554e7-ce7f-48f9-ad9b-46aa99d2b47d)

---
## 📖 Background and Access

CSF.Ind is a Shiny web app that enables users to calculate CSF indicators without prior coding knowledge. To access the app, run the following code in RStudio:

## How to Access the CSF.Ind shiny App
To view and interact with the Shiny app, simply click the link below:
[Open Shiny App](https://d1kw4k-cesar0ivan-alvites0diaz.shinyapps.io/csf_ind_app/).

## How to set Input Data for CSF.Ind shiny App
#### Data example
- To download an example dataset for computing growing stock, forest diversity indicators from multiple plots in the CSF.Ind web-based app, click [here](https://raw.githubusercontent.com/Cesarito2021/CSF.Ind/main/Data/Example_GS_FD.xlsx).  
- To download an example dataset for computing deadwood indicator from multiple plots in the CSF.Ind web-based app, click [here](https://raw.githubusercontent.com/Cesarito2021/CSF.Ind/main/Data/Example_deadwood.xlsx).  

#### Excel Template
- To download the input data template for computing the CSF.Ind web-based app, click [here](https://raw.githubusercontent.com/Cesarito2021/CSF.Ind/main/Data/InputData_Template.xlsx).  

#### Guideline 
- For more details about the required data for the app, download the guideline document [here](https://raw.githubusercontent.com/Cesarito2021/CSF.Ind/main/Data/InputData_GuideLine.pdf).  
Note: For a detailed description of the procedure and to view the repository, visit the project on GitHub. To learn how to use the app, watch the video tutorial (in progess).

## Logo
<div style="display: flex; justify-content: space-around; align-items: center;">
  <img src="https://github.com/user-attachments/assets/4a9a1811-4d3f-410a-acc9-6958f944c47e" alt="unimol_logo_png" width="200" />
  <img src="https://github.com/user-attachments/assets/8ceb07ba-c715-4c85-b1aa-ad20de6c8e22" alt="forwards_logo_png" width="200" />
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Shiny_hex_logo.svg/170px-Shiny_hex_logo.svg.png" alt="shinyapp_logo_png" width="200" />
  <img src="https://www.r-project.org/Rlogo.png" alt="shinyapp_logo_png" width="200" />  
</div>


## UI CSF.Ind
![Image](https://github.com/user-attachments/assets/4e01ff7c-37f3-458a-aa3e-87a7f18d951a)
Caption: A: Dashboard screen for selecting the CSF pillar.; B1-B2: Dashboard screen for the Mitigation and Adaptation Pillars. C1-C2: Representative images for both CSF pillars.
### 🚀 Overview
CSF.Ind calculates the following indicators for forest stands:
- Growing stock and carbon stock.
- Lying deadwood and standing deadwood.
- Structural and species diversity variables.
This web-based app is applicable to all European forests and is designed to support forest management under the Climate Smart Forestry (CSF) concept.

### 🎯 Objective
The primary objective of CSF.Ind is to provide a robust and user-friendly tool for calculating CSF indicators using validated procedures from the Forwards project. The app aims to:
- Simplify the calculation of CSF indicators for users without coding expertise.
- Support decision-makers in forest management under Sustainable Forest Management (SFM) criteria.
- Align with the Climate Smart Forestry (CSF) concept.

### 📚 Reference
The computation of CSF.Ind is based on local datasets and validated procedures. For more details, refer to the following publication:
- Alvites, C.; Santopuoli, G.; Tognetti, R. (In progress). CSF.Ind: A Shiny web-based application for generating climate smart forestry indicators in European forests using proximal data. Environmental Modelling & Software.

### 👨‍💻 Developers
Alvites Cesar
Post-Doctoral Researcher, University of Molise
Email: calvites1990@gmail.com

### 🙏 Acknowledgments
This web-based app was developed as part of post-doctoral research at the University of Molise by Cesar Alvites, within the framework of the Forwards project.
Learn more about the project: https://forwards-project.eu/

### 🛠️ CSF.Ind Web Application Configuration
#### Choose CSF Pillar
Users can select between two pillars:
- Mitigation: Includes growing stock, carbon stock, lying deadwood, standing deadwood, and an all-deadwood option (computed using double-pair diameter measurements).
- Adaptation: Includes 20 structural and species diversity indicators.
Table 1: Data source for CSF.Ind.

| Indicator                  | Input_Data                                                                 | Output_Data                                                                                                                                                               |
|:---------------------------|:--------------------------------------------------------------------------:|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| **1.1_GrowingStock** (*)   | IDPlot (order number), IDTree (order number), PlotArea (m²), TreeHeight (m), TreeDiameter (cm), TreeSpecies, ForManInt. | Vol_m3_ha: Forest volume regarding the volume of stem and large branches (diam. ≥ 5cm, m³/ha), AGB_tn_ha: Aboveground biomass (tons/ha).                                  |
| **1.2_CarbonStock** (*)    | IDPlot (order number), PlotArea (m²), StandVolume (m³/Tree), DomTreeSpecies, ForManInt. | AGB_tn_ha: Aboveground biomass (tons/ha), CS_tn_ha: Carbon Stock (tons/ha).                                                        |
| **1.3-1.5_Lying/Standing Deadwood** (*) | IDPlot (order number), PlotArea (m²), LengthLog (m), MinDiaLog (cm), MaxDiaLog (cm), Diameter Half-length (Dh05 cm), ForManInt. | LDT: Lying Deadwood Tree (m³/ha), CWD: Coarse Woody Debris (m³/ha), SDT: Standing Deadwood Tree (m³/ha), SNAG: Snag deadwood (m³/ha), All deadwood (m³/ha). |
| **2.1_ForestDiversity** (**) | IDPlot (order number), IDTree (order number), PlotArea (m²), TreeHeight (m), BasalArea (m²/tree), TreeSpecies, ForManInt. | 19 Diversity Indices: Mean_dbh (mean of diameters at tree height, cm), Mean_th (mean of tree heights, m), Sum_ba (sum of basal area, m²), N_sp (number of tree species), SI_dbh (Simpson index of diameters), SH_dbh (Shannon-Weiner index of diameters), SD_dbh (standard deviation of diameters), GI_ba (Gini index of basal area), CV_ba (coefficient of variation of basal area), SD_th (standard deviation of tree heights), CI_1000 (complex index at 1000m²), SHsp (Shannon-Weiner index of tree species), SH_th (Shannon-Weiner index of tree heights), SI_sp (Simpson index of tree species), SDI (Stand Density Index), TDD (Tree Diameter Diversity), THD (Tree Height Diversity), VEm (Vertical Evenness Index), VarDH (difference of diameter and height). |

#### Upload Excel File
Users can upload an Excel file (.xlsx) containing the required data for the selected CSF indicators. Refer to Table 1 in the app for the specific data requirements.

#### Forest Management Intensity (ForMainInt)
Users can choose whether to provide Forest Management Intensity information for each plot (Yes or No). If Yes is selected, users must specify the column containing this information.

#### Single vs. Multi-Plot Analysis
Users can choose between single-plot or multi-plot analysis (Yes or No). If Yes is selected, users must specify the column containing the PlotID information.

#### Requested Data
After selecting the CSF pillar and indicator, the app will display the required data inputs. Users can select the appropriate column names from a drop-down menu.

#### Download Outputs
- Download Table ForMainInt (.xlsx): Download the output values for Forest Management Intensity.
- Download Table Plot (.xlsx): Download the output values for individual plots.
- Download Report (.pdf): Download a PDF report summarizing the analysis.

#### Run Analysis
The CSF.Ind analysis is automatically computed once all required inputs are provided.

### 📥 Installation and Usage
Install R and RStudio (if not already installed).
Run the following code in RStudio to launch the app:

### 📜 License
This project is licensed under the GPL3 License. See the LICENSE file for details.

### 📧 Contact

For questions or feedback, please contact:
Cesar Alvites
Email: calvites1990@gmail.com
GitHub: Cesarito2021

---
