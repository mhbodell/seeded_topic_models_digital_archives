#----------------------------------------------------------------------------#
#                                 MAIN                                   ----
#----------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
# load packages
#---------------------------------------------------------------------------#
required_packages <- c("data.table", "dplyr", "ggplot2", "tidyverse", "cowplot", "gtools", "zoo", "knitr", "kableExtra", "RColorBrewer", "xtable", "lubridate", "lmtest", "broom", "ggpubr", "bayesplot", "rstanarm", "gridExtra", "psychometric", "bcp")

# Function to check if a package is installed, install if not, and then load it
load_or_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}

# Loop through the list of packages
for (package in required_packages) {
  load_or_install_package(package)
}

#---------------------------------------------------------------------------#
# set up
#---------------------------------------------------------------------------#

runs <- c('2021-06-06--18_11_45')

run_nr <- 1

where <- 'kb_new'

if(where=='kb_new'){
  base_path <- paste0('/home/miriam/Downloads/RunSuite',runs[run_nr],'/Run',runs[run_nr],'/Spalias/')
  save_path <- base_path
  git_path <- '/home/miriam/Documents/git/stm_media_framing_im/'
  data_path <- '/home/miriam/Documents/git/stm_media_framing_im/data/'
}else if(where=='miriam'){
  data_path <- '/Users/mhbodell/Documents/Work/VT19/media_group_threat/data/'
  # base_path <- paste0('/Volumes/One Touch/Run',runs[run_nr],'/')
  base_path <- paste0('/Users/mhbodell/Downloads/Run',runs[run_nr],'/')
  git_path <- '/Users/mhbodell/Documents/Work/VT19/media_group_threat/'
  save_path <- paste0('/Users/mhbodell/Downloads/Run',runs[run_nr],'/')
}else if(where=='nsc'){
  data_path <- '/proj/m4m/media_group_threat/data/'
  base_path <- paste0('/proj/m4m/Runs/RunSuite',runs[run_nr],'/Run',runs[run_nr],'/Spalias/')
  git_path <- '/proj/m4m/media_group_threat/'
  save_path <- paste0('/proj/m4m/model_output/Run',runs[run_nr],'/')
}

#---------------------------------------------------------------------------#
#                                read in data                            ----
#---------------------------------------------------------------------------#
# set threshold for "immigrant-rich"
too_small_threshold <- 0.025

# read in data & select docs based on threshold
data <- fread(paste0(base_path,'immigration_ts_doc_multi.csv'))
nrow(data)
data <- data[r>=too_small_threshold,]
nrow(data)
gc()
# add journal info
data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
                     ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
                            ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
table(data$paper)


#----------------------------------------------------------------------------#
#                    Identify high immigration salience weeks            ----
#----------------------------------------------------------------------------#

source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))
high_salience <- identify_high_salience(base_path = base_path)
events_df <- read_format_event_data(data_path = data_path)
#match event with high salience weeks
events_df_tmp2 <- events_df[events_df$ymw%in%high_salience$ymw | events_df$ymw2%in%high_salience$ymw ,]
events_df_tmp2[is.na(table_group),]
events_df_tmp2
gc()



#----------------------------------------------------------------------------#
#                   Case study: causal effect of events                  ----
#----------------------------------------------------------------------------#
source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))
# divide events by "event type"
hyp_terror_attack <- events_df_tmp2[table_group=='terrorist_attack',]
hyp_human_rights <- events_df_tmp2[table_group=='human_rights',]

# get weekly data
combined_data <- get_event_raw_data(run = runs[run_nr], data = data)
df_profile <- get_weekly_framing_salience(tmp_data = combined_data)

# set up model stuff
pre_weekly_tw <- 28
post_weekly_tw <- 14
pre_weeks_nr <- 4
post_weeks_nr <-2



# sweden terrorism
swe_terror <- bayes_fixed_effect_avg(frame_type = 'security',  event_data = hyp_terror_attack[event_names=='drottninggatan',],
                                     profile_data = df_profile,  pre_time_window = pre_weekly_tw, no_compare_frame = '',
                                     post_time_window = post_weekly_tw,   pre_nr_weeks = pre_weeks_nr,
                                     post_nr_weeks = post_weeks_nr, n_chains = 10, n_iter = 10000)
gc()

# human rights
alan_kurdi2 <- bayes_fixed_effect_avg(frame_type = 'humanitarian',  event_data = hyp_human_rights[event_names%in%c('alan_kurdi','löfven_speech'),],
                                      profile_data = df_profile,  pre_time_window = pre_weekly_tw, no_compare_frame = '',
                                      post_time_window = post_weekly_tw,   pre_nr_weeks = pre_weeks_nr,
                                      post_nr_weeks = post_weeks_nr,  n_chains = 10, n_iter = 10000)
gc()


#----------------------------------------------------------------------------#
#             Create Fig 3. event case studies (exponential)              ----
#----------------------------------------------------------------------------#
source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))

# generate plots
plt_terrorism_exp <- plot_event_study_exp_single(event0 = swe_terror[[1]]$data,  type0 = 'swe', col0 = '#c7bd36', event_name = 'Islamist terror')
plt_hum_exp <- plot_event_study_exp_single(event0 = alan_kurdi2[[1]]$data, type0= 'alan_kurdi2', col0 =  '#56B4E9', event_name = 'Human rights')


# combine plots
fe_plot_exp <-  ggpubr::ggarrange(plt_terrorism_exp + theme_bw(base_size = 14) + theme(legend.position = 'none', title = element_text(size= 10)), 
                                  plt_hum_exp + theme_bw(base_size = 14) + theme(legend.position = 'none', title = element_text(size= 10)), #
                                  labels = c("A",'B'),
                                  nrow = 1, ncol = 2,font.label = list(size = 11,face = "plain"))
fe_plot_exp
too_small_threshold_t <-gsub(as.character(too_small_threshold), pattern = '\\.', replacement = '_')
ggsave(fe_plot_exp, 
       file = paste0(save_path, too_small_threshold_t,'/event_study_validation_RR.png'),
       height = 12,
       width = 15,
       units = 'cm')

gc()


#----------------------------------------------------------------------------#
#                       Exponential effects                       ----
#----------------------------------------------------------------------------#
terrorism_eff <- exponentiate_event_effects_single(event0 = swe_terror[[1]]$data,type0 = 'swe')
terrorism_eff
human_eff <- exponentiate_event_effects_single(event0 = alan_kurdi2[[1]]$data, type0 = 'alan_kurdi2')
human_eff

exp_effect <- rbind(terrorism_eff, human_eff)

fwrite(exp_effect, paste0(save_path, too_small_threshold_t,'/exp_fe_',too_small_threshold_t,'_intercept_validation_RR.csv'))

