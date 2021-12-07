# Reproduction

This folder contains all necessary data, scripts and instructions to *fully reproduce* the results of the study. This is a simulation study building on semi-synthetic data, therefore, all results should be reproducible using only the non-synthetic data source, the developed R-code and the indicated R-packages.

For easier and speedier reproduction of certain parts, we have uploaded interim data objects to a cloud service. This makes *partial reproductions* possible. Unfortunately, we cannot create a simple link to share in this repo because the Utrecht University administrators only allow sharing the folders with specified people. Therefore, please just write an [email](marcoramljak@googlemail.com) and the folder will be shared with you, without any restrictions.

This study follows the logics of the MNO-simulator workflow, see the figure below. For this we developed four scripts, two regular `\*.R` scripts and two `\*.Rmd` notebooks. We have chosen the notebook format for the main simulation steps as this format allows for easy combination of code, results/plots and description/interpretation.

-   `pipeline functions.R`: Contains all developed custom functions and needs to be preloaded via `source()` for the further notebooks to work. This is basically the heart of the MNO-simulator workflow.

-   `Munich_generate.R`: Processes the raw census data to make it manageable for the the three modules.

-   `Generation of network scenarios.Rmd`: Generates the mobile phone population and the four network scenarios, as well as performs the phone-to-cell association task (module: Generation). This notebook is also the basis for the supplementary material, which can be found [here](.)

-   `Model_mismatch.Rmd`: Here we develop all necessary estimation models and run the different deterministic and probabilistic estimators (module: Estimation). Furthermore, we evaluate the results by means of KWD and create the final result plots (module: Evaluation).

![Analysis plan according to the MNO-simulator workflow](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/analysis%20plan.png)

## For a **full reproduction**, please execute the following steps:

-   Clone/Fork/Download this repository.

-   Download the [2011 German census data on a regular grid](https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=2447684734B1934C8AD8042D9236B37A.live931?__blob=publicationFile&v=2). Further info on the file can be found [here](https://www.zensus2011.de/DE/Home/Aktuelles/DemografischeGrunddaten.html?nn=3065474).

-   Unzip the downloaded file into the [Data](https://github.com/R-ramljak/MNO_uncertainty/tree/main/Reproduction/Data) folder.

-   If RStudio is available, open the [MNO_uncertainty.Rproj](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/MNO_uncertainty.Rproj) file for easier path management.

-   Run the the [Munich_generate.R](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/Munich_generate.R) script. Make sure the object [munich.rds]() is saved in the Data folder.

-   Make sure the object `munich.rds` is saved. For convenience purposes, the folder already contains this processed [file](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/Data/munich.rds).

-   Run the notebook script [Generation of network scenarios](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/Generation%20of%20network%20scenarios.Rmd). Here we generate the network scenarios. Make sure the object `gen.model.objects` is saved.

-   Run the notebook script [Model_mismatch](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/Model_mismatch.Rmd). Here we run the estimations and evaluate the results.

Depending on the fact if you use the RStudio project file, file path adjustment is necessary.

## For a **partial reproduction**, please execute the following steps:

-   Clone/Fork/Download this repository.

-   Make sure that you have access to the interim data objects and copy them into the predefined Data folder.

-   If RStudio is available, open the [MNO_uncertainty.Rproj](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/MNO_uncertainty.Rproj) file for easier path management.

-   Depending on which results you want to reproduce or which other code blocks you want to experiment with, you can follow this [file](https://github.com/R-ramljak/MNO_uncertainty/blob/main/Reproduction/Data/interim%20objects%20list.txt). It describes, which `*.rds` object contains which information and where it is loaded into one of the two main notebooks.

## License

We want to encourage reproduction and further experimentation. Please let us know if you encounter mistakes or problems.

|           |                                                                                                                                                             |
|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *authors* | Ramljak M.                                                                                                                                                  |
| *version* | 1.0                                                                                                                                                         |
| *status*  | 2021 - closed                                                                                                                                               |
| *license* | [EUPL](https://joinup.ec.europa.eu/sites/default/files/custom-page/attachment/eupl_v1.2_en.pdf) *(concerning the source code, please cite this repository)* |
