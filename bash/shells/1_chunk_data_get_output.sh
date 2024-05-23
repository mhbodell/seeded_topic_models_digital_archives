#!/bin/sh



#SBATCH -J "chunk_output.sh"
#SBATCH -t 8:00:00
#SBATCH -A naiss2023-22-1050
#SBATCH -N 1
#SBATCH -C fat
#SBATCH --exclusive
#SBATCH --mail-user=miriam.hurtado.bodell@liu.se
#SBATCH --mail-type=ALL

projinfo

#-----------------------
# Load modules
#-----------------------
module add Python/3.10.4-env-hpc1-gcc-2022a-eb
module add R/4.2.2-hpc1-gcc-11.3.0-bare


#--------------------------
# DRAW 1
echo draw1
#----------------------------



echo part1
#-----------------------
# PART 1 (python)
#------------------------

export input_file='z_2200.csv'
export run_nr='2024-02-08--09_09_45'

# chunk data in python (save smaller matrices in multiple files)
# MANUALLY CREATE FOLDER TO STORE DATA
python /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/chunk_data.py

#-----------------------
# PART 2 (R)
#------------------------
echo part2
# set up model specific variables
_proj_name=$'efe_et'
_run_nr=$'2024-02-08--09_09_45'

# clean chunks in R
Rscript /proj/efe_etseeded_topic_models_digital_archives/code/model_output/get_model_output/clean_chunked_data.R ${_proj_name} ${_run_nr}


#-----------------------
# PART 3 (Get output summary)
#------------------------
echo part3
#echo part3 set up model specific variables
_run=$'2024-02-08--09_09_45'
_proj_name=$'efe_et'
_threshold=$'0.01'

#-----------
# REAL OUTPUT
# before running, need to check the model to manually change some difficult topics!
Rscript /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/naiss_get_output_cooc_multiseed.R ${_run} ${_proj_name} ${_threshold}



#--------------------------
# DRAW 2
echo draw2
#----------------------------


echo part1
#-----------------------
# PART 1 (python)
#------------------------

export input_file='z_2150.csv'
export run_nr='2024-02-08--09_09_45'

# chunk data in python (save smaller matrices in multiple files)
python /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/chunk_data.py

#-----------------------
# PART 2 (R)
#------------------------
echo part2
# set up model specific variables
_proj_name=$'efe_et'
_run_nr=$'2024-02-08--09_09_45'

# clean chunks in R
Rscript /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/clean_chunked_data.R ${_proj_name} ${_run_nr}


#-----------------------
# PART 3 (Get output summary)
#------------------------
echo part3
#echo part3 set up model specific variables
_run=$'2024-02-08--09_09_45'
_proj_name=$'efe_et'
_threshold=$'0.01'

#-----------
# REAL OUTPUT
# before running, need to check the model to manually change some difficult topics!
Rscript /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/naiss_get_output_cooc_multiseed_z2.R ${_run} ${_proj_name} ${_threshold}




#--------------------------
# DRAW 3
echo draw3
#----------------------------

echo part1
#-----------------------
# PART 1 (python)
#------------------------

export input_file='z_2100.csv'
export run_nr='2024-02-08--09_09_45'

# chunk data in python (save smaller matrices in multiple files)
python /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/chunk_data.py

#-----------------------
# PART 2 (R)
#------------------------
echo part2
# set up model specific variables
_proj_name=$'efe_et'
_run_nr=$'2024-02-08--09_09_45'

# clean chunks in R
Rscript /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/clean_chunked_data.R ${_proj_name} ${_run_nr}


#-----------------------
# PART 3 (Get output summary)
#------------------------
echo part3
#echo part3 set up model specific variables
_run=$'2024-02-08--09_09_45'
_proj_name=$'efe_et'
_threshold=$'0.01'

#-----------
# REAL OUTPUT
# before running, need to check the model to manually change some difficult topics!

Rscript /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/naiss_get_output_cooc_multiseed_z3.R ${_run} ${_proj_name} ${_threshold}



#-----------------------
# COMBINE ALL DRAWS
#------------------------
echo part4
#echo part3 set up model specific variables
_run=$'2024-02-08--09_09_45'
_proj_name=$'efe_et'

#-----------
# REAL OUTPUT
Rscript /proj/efe_et/seeded_topic_models_digital_archives/code/model_output/get_model_output/naiss_get_multiple_posterior_draws.R ${_run} ${_proj_name}





