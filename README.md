# Dissertation
 
This repository contains replication files for my [PhD dissertation (PDF here)](https://github.com/clanfear/Dissertation/blob/main/projects/diss_doc/index/_book/thesis.pdf):

> Lanfear, Charles C. 2021. "Integrating Collective Efficacy and Criminal Opportunity: Disorder, the Built Environment, and Policing." PhD Dissertation, Department of Sociology, University of Washington.

Replication files are located in `projects` and are divided by chapter. These are the relevant directories:

* `diss_doc`: Files necessary to build the PDF of the final dissertation. This is based on [Ben Marwick's `huskydown` package](https://github.com/benmarwick/huskydown) for constructing R Markdown based University of Washington theses and dissertations.
* `defense`: Slides for my dissertation defense and the files necessary to build them.
* `built_environment`: Files necessary to run analyses for the second chapter, "Collective Efficacy and the Built Environment."
* `formal_control`: Files necessary to run analyses for the third chapter, "Collective Efficacy and Formal Social Control."
* `shared`: Files necessary to gather and prepare raw data for both empirical chapters.

To replicate the empirical chapters it is necessary to run the scripts in `shared` first. [There is a convenience script to accomplish this found here.](https://github.com/clanfear/Dissertation/blob/main/projects/shared/syntax/chicago/00_build_shared_data.R) This requires all data sets used in this project. They are documented in that same script, but not included if they are either (1) access restricted, such as the PHDCN and CCAHS releases from ICPSR or (2) prohibitively large, as is the case for the Chicago Police Department crime data. Other datasets will be automatically downloaded when the shared data are built (e.g., census data).