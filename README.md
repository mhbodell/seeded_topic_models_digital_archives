
# "Seeded Topic Models in Digital Archives: Analyzing Interpretations of Immigration in Swedish Newspapers, 1945-2019" 

This is the code needed to reproduce the paper Hurtado Bodell, M., Keuschnigg, M., & Magnusson, M. "Seeded Topic Models in Digital Archives: Analyzing Interpretations of Immigration in Swedish Newspapers, 1945-2019".

If you want to recreate the model from scratch go through step 1-3. If you just want to re-run the code that is used for the main analysis in the paper, jump to step 4.


## 1. Installing Java implementation of seeded topic model with scalable Gibbs sampler

The paper utilize the sampler introduced in Magnusson et al (2018) "Sparse partially collapsed mcmc for parallel inference in topic models". You find the Java implementation used in this paper `[here](https://github.com/lejon/PartiallyCollapsedLDA)`

Follow the instructions in the PartiallyCollapsedLDA repo to make install the sampler used in the paper.


## 2. Run the main seeded topic model using a scalable Gibbs sampler

Use the `[bash file](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/shells/0_run_main_model.sh)` used to run the main model. The `[confihuration file](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/configs/main_model_k1000.cfg)` used for the main analysis specifies the model parameters (k = 1000, alpha = 0.5, beta = 0.01) and number of iterations (2200), and most importantly the `[seed wod list](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/priors/priors_main.txt)` used to guide the model.

Make sure to update config file and bash file to fit your local environment.

## 3. Transform raw model output to data for analysis

Use the `[bash file](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/bash/shells/1_chunk_data_get_output.sh)` used to transform the raw model output to a R-readable format, make sure to update file paths to fit your local environment.


## 4. Run the main analysis

Run `[main.R](https://github.com/mhbodell/seeded_topic_models_digital_archives/blob/main/code/in_paper_analysis/main.R)` to get the figures and numbers included in the main text of the paper. The code in this script creates (1) Figure 1; immigration salience vs. real immigration numbers over time, and (2) Figure 2; the relative frame salience over time and identify and plot turning points in the immigration discourse.

You find the code used to create the figures and numbers in the appendix `[here](https://github.com/mhbodell/seeded_topic_models_digital_archives/tree/main/code/in_paper_analysis/supplementary)`

## Contact

Miriam Hurtado Bodell: `[miriam.hurtado.bodell@liu.se](mailto:miriam.hurtado.bodell@liu.se)`



