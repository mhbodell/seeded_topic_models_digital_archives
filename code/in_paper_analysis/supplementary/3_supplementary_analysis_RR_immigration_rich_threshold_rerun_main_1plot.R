#----------------------------------------------------------------------------#
#           RE-RUN  MAIN TO TEST SENSITIVITY OF SEED WORDS                ----
#----------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
# load packages
#---------------------------------------------------------------------------#
required_packages <- c("data.table", "dplyr", "ggplot2", "tidyverse", "cowplot", 
                       "gtools", "zoo", "knitr", "kableExtra", "RColorBrewer",
                       "xtable", "lubridate", "lmtest", "broom", "ggpubr", 
                       "bayesplot", "rstanarm", "gridExtra", "psychometric", "bcp")

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

#runs <- c('2024-01-11--16_01_43','2024-01-22--00_44_20','2024-01-27--11_21_40')
#run_nr <- length(runs)
#run <- runs[run_nr]

where <- 'naiss'
store_figs <- list()
validations <- c('90_1','90_2','90_3','80_1','80_2','80_3','70_1','70_2','70_3')
for(ii in 1:length(validations)){
  if(where=='naiss'){
    run <- validations[ii]
    print(run)
    data_path <- '/proj/efe_et/old_m4m/media_group_threat/data/'
    base_path <- paste0('/proj/efe_et/model_output/seed_validation_models/',run,'/')
    git_path <- '/proj/efe_et/old_m4m/media_group_threat/'
    save_path <- paste0('/proj/efe_et/model_output/seed_validation_models/') } 
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
  #events_df_tmp2[is.na(table_group),]
  events_df_tmp2
  gc()
  
  #----------------------------------------------------------------------------#
  #                 Create Fig. 2a (framing salience)                   ----
  #----------------------------------------------------------------------------#
  source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))
  # get data of framing salience (date-level)
  df_frame_salience <- get_framing_salience(current_model = run, data = data)
  
  # aggregate data to yearly-level
  df_frame_salience_yearly <- get_yearly_framing_salience(df_frame_salience_plot = copy(df_frame_salience))
  
  # create colorblind friendly colors
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000", "seagreen","darkred")
  
  # control that all frames are in the data
  table(df_frame_salience_yearly$frame)
  
  # plot time series of frame salience
  plt_frame_ts <- plot_framing_salience(df_plot = df_frame_salience_yearly)

  #----------------------------------------------------------------------------#
  #                 Create Fig. 2b (turning points)                   ----
  #----------------------------------------------------------------------------#
  set.seed(14121)
  
  # cast to wide format
  df_frame_salience_yearly_wide <- pivot_wider(df_frame_salience_yearly, id_cols = year, names_from = frame, values_from = frame_r)
  
  # run multivariate turning point analysis
  multivariate_bcp <- get_multivariate_tp(tmp_data = df_frame_salience_yearly_wide)
  
  # run univariate turning point analysis per frame
  uni_security <- get_univariate_tp(tmp_data = df_frame_salience_yearly_wide, frame_name = 'security')
  uni_politics <- get_univariate_tp(tmp_data = df_frame_salience_yearly_wide, frame_name = 'politics')
  uni_cultural <- get_univariate_tp(tmp_data = df_frame_salience_yearly_wide, frame_name = 'cultural')
  uni_economy <- get_univariate_tp(tmp_data = df_frame_salience_yearly_wide, frame_name = 'economy')
  uni_hr <- get_univariate_tp(tmp_data = df_frame_salience_yearly_wide, frame_name = 'humanitarian')
  
  # combine univariate frames
  #tmp <- rbind(tmp_security, tmp_economy, tmp_humanitarian,tmp_cultural,tmp_politics)
  univariate_bcp <-rbind(uni_security, uni_politics, uni_cultural, uni_economy, uni_hr)
  gc()
  
  # change name of levels (to make plot pretty)
  univariate_bcp$frame <- factor(univariate_bcp$frame, levels = c("humanitarian", "security", "economy", 'cultural','political'))
  
  # which years have highest change points probability
  tp_year <- multivariate_bcp[multivariate_bcp$posterior_prob>0.5,]$year
  # change x-axis labels 1964-1966 --> 1965 to make it look prettier
  tp_year <- c(tp_year[!tp_year%in%c(1964,1966)], 1965)
  tp_year <- tp_year[order(tp_year)]
  
  # create plot
  plot_tp <- plot_turning_points(df_plot = univariate_bcp, turning_points =  tp_year, run = "2021-06-06--18_11_45")
  
  # add lines for turning points in time series of frame salience
  plt_frame_ts + geom_vline(xintercept = tp_year , lty = 'dotted')  -> plt_frame_ts2
  
  # create common legend
  mylegend <- g_legend(plt_frame_ts)
  
  # combine Fig. 2a &  Fig. 2 
  fig2<- ggpubr::ggarrange(plt_frame_ts2 + theme(axis.title.y = element_text(size = 6)) +  theme(legend.position="none"), 
                                 plot_tp + theme(axis.title.y = element_text(size = 6)), 
                                 nrow = 2, ncol = 1, labels = c('',''),
                                 font.label = list(face = 'plain', size = 8), common.legend = F) 
  
  store_figs[[ii]] <- fig2
}



# combine Fig. 2a &  Fig. 2 
different_seeds_plot <- ggpubr::ggarrange(store_figs[[1]] , #90
                                               store_figs[[2]],
                                               store_figs[[3]],
                                               store_figs[[4]],
                                               store_figs[[5]],
                                               store_figs[[6]],
                                               store_figs[[7]],
                                               store_figs[[8]],
                                               store_figs[[9]], 
                                               nrow = 3, ncol = 3, 
                                               labels = c('A','B','C','D','E','F','G','H','I'),
                                               font.label = list(face = 'plain', size = 11), 
                                               common.legend = T) 
ggsave(different_seeds_plot,  file = paste0(save_path, 'validation_seed_words_main.png'),
       height = 30, width = 32,  units = 'cm')


