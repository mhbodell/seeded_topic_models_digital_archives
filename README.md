# Seeded Topic Models in Digital Archives: Analyzing Interpretations of Immigration in Swedish Newspapers, 1945-2019

This repository contains the code needed to reproduce the paper by Hurtado Bodell, M., Keuschnigg, M., & Magnusson, M., (2024) "Seeded Topic Models in Digital Archives: Analyzing Interpretations of Immigration in Swedish Newspapers, 1945-2019." [conditionally accepted in Sociological Methods & Research]

If you want to recreate the model from scratch, follow steps 1-3. If you just want to re-run the code used for the main analysis in the paper, skip to step 4.

## 1. Installing the java implementation of the seeded topic model with a scalable Gibbs sampler

The paper utilizes the sampler introduced in Magnusson et al. (2018) "Sparse Partially Collapsed MCMC for Parallel Inference in Topic Models." You can find the Java implementation used in this paper [here](https://github.com/lejon/PartiallyCollapsedLDA).

Follow the instructions in the PartiallyCollapsedLDA repository to install the sampler used in the paper.

## 2. Running the main seeded topic model using a scalable Gibbs sampler

Use this [bash file](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/shells/0_run_main_model.sh) to run the main model. The [configuration file](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/configs/main_model_k1000.cfg) used for the main analysis specifies the model parameters (k = 1000, alpha = 0.5, beta = 0.01), the number of iterations (2200), and most importantly, the [seed word list](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/priors/priors_main.txt) used to guide the model.

Make sure to update the configuration file and bash file to fit your local environment.

The same configuration file can be used used to run models with k = 950 and k = 1050, just change the hyperparameter k by changing `topics = 950` or `topics = 1050` in the configuration file. These models are used to create Figure S4 in the appendix. Moreover, to replicate the models used to validate our seed words (i.e. replicate Fig.). You find the different prior lists used to conduct the seed word validation in Figure S7 in the paper appendix [here](https://github.com/mhbodell/seeded_topic_models_digital_archives/tree/main/bash/priors/seed_word_validation_priors). 

## 3. Transforming the raw model output to data for analysis

Use this [bash file](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/shells/1_chunk_data_get_output.sh) to transform the raw model output into an R-readable format. Make sure to update the file paths to fit your local environment.

## 4. Running the main analysis

Run [main.R](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/code/in_paper_analysis/main.R) to generate the figures and numbers included in the main text of the paper. The code in this script creates (1) Figure 1: immigration salience vs. real immigration numbers over time, and (2) Figure 2: the relative frame salience over time, identifying and plotting turning points in the immigration discourse.

All data needed to run main.R are available [here](https://github.com/mhbodell/seeded_topic_models_digital_archives/tree/main/data) and you find all the figures from the paper [here](https://github.com/mhbodell/seeded_topic_models_digital_archives/tree/main/output/0_025).

The figures produced by main.R are

You can find the code used to create the figures and numbers in the appendix [here](https://github.com/mhbodell/seeded_topic_models_digital_archives/tree/main/code/in_paper_analysis/supplementary).

## Contact

Miriam Hurtado Bodell: [miriam.hurtado.bodell@liu.se](mailto:miriam.hurtado.bodell@liu.se)
