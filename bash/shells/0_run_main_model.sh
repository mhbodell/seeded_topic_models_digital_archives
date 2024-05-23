#!/bin/sh


#SBATCH -J "main model"
#SBATCH -t 160:00:00
#SBATCH -A snic2021-5-161
#SBATCH -N 1
#SBATCH -C fat
#SBATCH --exclusive
#SBATCH --mail-user=miriam.hurtado.bodell@liu.se
#SBATCH --mail-type=ALL


echo projinfo

# choose java version
ml Java/1.8.0_74-nsc1


# Run Models
java  -Xmx350g -Xms350g -cp nsc-test/PartiallyCollapsedLDA-8.4.0/target/PCPLDA-8.4.0.jar cc.mallet.topics.tui.ParallelLDA --run_cfg=seeded_topic_models_digital_archives/bash/configs/main_model_k1000.cfg




