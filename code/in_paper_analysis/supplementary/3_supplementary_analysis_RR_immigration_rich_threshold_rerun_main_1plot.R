#----------------------------------------------------------------------------#
#           RE-RUN  MAIN TO TEST SENSITIVITY OF SEED WORDS                ----
#----------------------------------------------------------------------------#
# DUE TO DATA STORAGE LIMITATIONS ON GITHUB WE DO NOT PROVIDE THE DATA
# FOR THIS ROBUSTNESS ANALYSIS

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
store_figs <- list()
validations <- c(0.01,0.04,0.05)

git_path <- paste0('/home/miriam/Documents/git/seeded_topic_models_digital_archives/')
save_path <- paste0(git_path, 'output/')
data_path <- paste0(git_path,'data/')
source(file = paste0(git_path,'code/in_paper_analysis/utils_funcs.R'))

for(ii in 1:length(validations)){
    run <- '2021-06-06--18_11_45'; print(ii)
    data <- fread(paste0('/home/miriam/Downloads/RunSuite',run,'/Run',run,'/Spalias/immigration_ts_doc_multi.csv'))
   # set threshold for "immigrant-rich"
  too_small_threshold <- validations[ii]
  # read in data & select docs based on threshold
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
  #                 Create Fig. 2a (framing salience)                   ----
  # get data of framing salience (date-level)
  df_frame_salience <- get_framing_salience(current_model = run, data = data)
  # aggregate data to yearly-level
  df_frame_salience_yearly <- get_yearly_framing_salience(df_frame_salience_plot = copy(df_frame_salience))
  # plot time series of frame salience
  plt_frame_ts <- plot_framing_salience(df_plot = df_frame_salience_yearly)
  #----------------------------------------------------------------------------#
  #                 Create Fig. 2b (turning points)                   ----
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
  univariate_bcp <-rbind(uni_security, uni_politics, uni_cultural, uni_economy, uni_hr)
  gc()
  # change name of levels (to make plot pretty)
  univariate_bcp$frame <- factor(univariate_bcp$frame, levels = c("humanitarian", "security", "economy", 'cultural','politics'))
  # which years have highest change points probability
  tp_year <- multivariate_bcp[multivariate_bcp$posterior_prob>0.5,]$year
  # change x-axis labels 1964-1966 --> 1965 to make it look prettier
  tp_year <- c(tp_year[!tp_year%in%c(1964,1966)], 1965)
  tp_year <- tp_year[order(tp_year)]
  # create plot
  plot_tp <- plot_turning_points_valid(df_plot = univariate_bcp, turning_points =  tp_year, run = run)
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
different_thres_plot <- ggpubr::ggarrange(store_figs[[1]] ,
                                               store_figs[[2]],
                                               store_figs[[3]], 
                                               nrow = 3, ncol = 1, 
                                               labels = c('A','B','C'),
                                               font.label = list(face = 'plain', size = 11), 
                                               common.legend = T) 
ggsave(different_thres_plot,  file = paste0(save_path, '0_025/validation_thresholds_main.png'),
       height = 25, width = 15,  units = 'cm')


