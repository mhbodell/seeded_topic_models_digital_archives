#!/bin/sh


#SBATCH -J "main model - CHANGE THIS DEPENDING ON HOW YOU RUN BASH FILE"
#SBATCH -t 160:00:00
#SBATCH -A snic2021-5-161
#SBATCH -N 1
#SBATCH -C fat
#SBATCH --exclusive


# choose java version - specific for my environment
ml Java/1.8.0_74-nsc1


# Run Models
java  -Xmx350g -Xms350g -cp target/PCPLDA-8.4.0.jar cc.mallet.topics.tui.ParallelLDA --run_cfg=seeded_topic_models_digital_archives/bash/configs/main_model_k1000.cfg




