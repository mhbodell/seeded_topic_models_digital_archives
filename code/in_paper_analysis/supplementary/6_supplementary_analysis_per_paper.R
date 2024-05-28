#----------------------------------------------------------------------------#
#                        SUPPLEMENTARY MATERIAL                        ----
#----------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
# load packages
#---------------------------------------------------------------------------#
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gtools)
library(zoo)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(xtable)
library(lubridate)
library(lmtest)
library(broom)
library(ggpubr)
library(bayesplot)
library(rstanarm)
library(gridExtra)
library(psychometric)
library(bcp)

#---------------------------------------------------------------------------#
# set up
#---------------------------------------------------------------------------#

runs <- c('2021-06-06--18_11_45','2021-05-26--13_50_02','2021-06-24--09-02-42')
run_nr <- 1
where <- 'kb_new'

if(where=='kb_new'){
  base_path <- paste0('/home/miriam/Downloads/RunSuite',runs[run_nr],'/Run',runs[run_nr],'/Spalias/')
  save_path <- base_path
  git_path <- '/home/miriam/Documents/git/media_group_threat/'
  data_path <- '/home/miriam/Documents/git/media_group_threat/data/'
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
too_small_threshold_t <- gsub(pattern = '\\.','_',as.character(too_small_threshold))

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


#---------------------------------------------------------------------------#
#             Run analysis for specific paper                            ----
#---------------------------------------------------------------------------#
# create object to store figs in
paper_figs <- list()



# set which paper
tmp_paper <- 'Svenska Dagbladet'
dataP <- data[paper==tmp_paper,]


#----------------------------------------------------------------------------#
#                 Create Fig. 2a (framing salience)                   ----
#----------------------------------------------------------------------------#
source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))
# get data of framing salience (date-level)
df_frame_salience <- get_framing_salience(run = runs[run_nr], data = dataP)

# aggregate data to yearly-level
df_frame_salience_yearly <- get_yearly_framing_salience(df_frame_salience_plot = copy(df_frame_salience))

# create colorblind friendly colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000", "seagreen","darkred")

# control that all frames are in the data
table(df_frame_salience_yearly$frame)

# plot time series of frame salience
plt_frame_ts <- plot_framing_salience(df_plot = df_frame_salience_yearly)
plt_frame_ts

#----------------------------------------------------------------------------#
#                 Create Fig. 2b (turning points)                   ----
#----------------------------------------------------------------------------#
source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))
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

# turn x-labels
plot_tp <- plot_tp + theme(axis.text.x=element_text(angle=90, hjust=1))

# combine Fig. 2a &  Fig. 2 
fig2 <- ggpubr::ggarrange(plt_frame_ts2, plot_tp , nrow = 2, ncol = 1, labels = c('',''),
                          font.label = list(face = 'plain', size = 11), common.legend = T) 
fig2
too_small_threshold_t <-gsub(as.character(too_small_threshold), pattern = '\\.', replacement = '_')
posthoc_type <- 'minimal'
tmp_file_name <- paste0(save_path, too_small_threshold_t,'/cp_eras_',too_small_threshold_t,'_',posthoc_type,'0_5thres_',tmp_paper,'.png')
ggsave(fig2,  file = tmp_file_name,
       height = 12, width = 15,  units = 'cm')

# store paper-specific plot
paper_figs[[tmp_paper]] <- fig2

#combine paper-specific plots
tmp_file_name <- paste0(save_path, too_small_threshold_t,'/cp_eras_',too_small_threshold_t,'_',posthoc_type,'0_5thres_papers_.png')
paper_main_plot <-  ggpubr::ggarrange(paper_figs[['Aftonbladet']] + theme(legend.position = "none", title = element_text(size= 5)) + theme_bw(base_size = 7), 
                                      paper_figs[['Expressen']] + theme_bw(base_size = 7) + theme(legend.position = "none", title = element_text(size=5)), 
                                      paper_figs[['Dagens Nyheter']]+ theme_bw(base_size = 7) + theme(legend.position = "none", title = element_text(size= 5)), #
                                      paper_figs[['Svenska Dagbladet']] + theme_bw(base_size = 7) + theme(legend.position = "none", title = element_text(size= 5)) , 
                                  labels = c("A",'B', "C",'D'), common.legend = TRUE,
                                  nrow = 2, ncol = 2,font.label = list(size = 11,face = "plain"))
paper_main_plot
ggsave(paper_main_plot, 
       file = tmp_file_name,
       height = 20,
       width = 28,
       units = 'cm')

gc()



