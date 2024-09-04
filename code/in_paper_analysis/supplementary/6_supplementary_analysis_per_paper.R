#----------------------------------------------------------------------------#
#                        SUPPLEMENTARY MATERIAL                        ----
#                            anlysis per paper              
#----------------------------------------------------------------------------#

#---------------------------------------------------------------------------#
# install and load packages
#---------------------------------------------------------------------------#
required_packages <- c("data.table", "dplyr", "ggplot2", "tidyverse", "cowplot", 
                       "gtools", "zoo", "knitr", "kableExtra", "RColorBrewer", 
                       "xtable", "lubridate", "lmtest", "broom", "ggpubr", 
                       "bayesplot", "rstanarm", "gridExtra", "psychometric", 
                       "bcp") 
# bcp may need to be installed from https://cran.r-project.org/src/contrib/Archive/bcp/

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
# set-up (choose model and set paths to files/where to save)
#---------------------------------------------------------------------------#

# change these to fit your model
runs <- c('2021-06-06--18_11_45')
run_nr <- 1
where <- 'kb_new'
if(where=='kb_new'){
  # change paths to your local path
  base_path <- paste0('/home/miriam/Documents/git/seeded_topic_models_digital_archives/')
  save_path <- paste0(base_path, 'output/')
  data_path <- paste0(base_path,'data/')
}

# load own functions needed in the analysis
source(file = paste0(base_path,'code/in_paper_analysis/utils_funcs.R'))


#---------------------------------------------------------------------------#
#                                read in data                            ----
#---------------------------------------------------------------------------#
# set threshold for "immigrant-rich"
too_small_threshold <- 0.025
too_small_threshold_t <- gsub(pattern = '\\.','_',as.character(too_small_threshold))

# read in data & select docs based on threshold
# need original data, that is too large to store on github
data <- fread(paste0('/home/miriam/Downloads/RunSuite',runs,'/Run',runs,'/Spalias/immigration_ts_doc_multi.csv'))
data <- data[r>=too_small_threshold,]

# add mewspaper info
data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
                     ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
                            ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
table(data$paper)

#---------------------------------------------------------------------------#
#             Run analysis for specific paper                            ----
#---------------------------------------------------------------------------#
# create object to store figs in
paper_figs <- list()

# set which paper
papers <- c('Aftonbladet','Dagens Nyheter','Expressen', 'Svenska Dagbladet')
for(tmp_paper in papers){ print(tmp_paper)
  dataP <- data[paper==tmp_paper,]

  # get data of framing salience (date-level)
  df_frame_salience <- get_framing_salience(current_model = runs[run_nr], data = dataP)
  
  # aggregate data to yearly-level
  df_frame_salience_yearly <- get_yearly_framing_salience(df_frame_salience_plot = copy(df_frame_salience))
  
  # control that all frames are in the data
  table(df_frame_salience_yearly$frame)
  
  # plot time series of frame salience
  plt_frame_ts <- plot_framing_salience(df_plot = df_frame_salience_yearly)
  plt_frame_ts
  
  #----------------------------------------------------------------------------#
  #                 Create Fig. 2b (turning points)                   ----
  #----------------------------------------------------------------------------#
  # cast to wide format
  df_frame_salience_yearly_wide <- pivot_wider(df_frame_salience_yearly, id_cols = year, 
                                               names_from = frame, values_from = frame_r)
  
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
  plot_tp <- plot_turning_points(df_plot = univariate_bcp, turning_points =  tp_year, run = "2021-06-06--18_11_45")
  
  # add lines for turning points in time series of frame salience
  plt_frame_ts + geom_vline(xintercept = tp_year , lty = 'dotted')  -> plt_frame_ts2
  
  # create common legend
  mylegend <- g_legend(plt_frame_ts)
  
  # combine Fig. 2a &  Fig. 2 
  fig2 <- ggpubr::ggarrange(plt_frame_ts2, plot_tp , nrow = 2, ncol = 1, labels = c('',''),
                            font.label = list(face = 'plain', size = 11), common.legend = T) 
  fig2

  # store paper-specific plot
  paper_figs[[tmp_paper]] <- fig2
  

}

#combine paper-specific plots
tmp_file_name <- paste0(save_path, too_small_threshold_t,'/validation_split_newspapers.pdf')
split_paper_fig2 <-  ggpubr::ggarrange(paper_figs[['Aftonbladet']] + theme_bw(base_size = 7)+ theme(legend.position = "none", title = element_text(size= 5)),  
                                      paper_figs[['Expressen']] + theme_bw(base_size = 7) + theme(legend.position = "none", title = element_text(size=5)), 
                                      paper_figs[['Dagens Nyheter']]+ theme_bw(base_size = 7) + theme(legend.position = "none", title = element_text(size= 5)), #
                                      paper_figs[['Svenska Dagbladet']] + theme_bw(base_size = 7) + theme(legend.position = "none", title = element_text(size= 5)) , 
                                      labels = c("A",'B', "C",'D'), #common.legend = TRUE,
                                      nrow = 2, ncol = 2,font.label = list(size = 11,face = "plain"))


split_paper_fig2
ggsave(split_paper_fig2, file = tmp_file_name, height = 23, width = 32, units = 'cm')
gc()


