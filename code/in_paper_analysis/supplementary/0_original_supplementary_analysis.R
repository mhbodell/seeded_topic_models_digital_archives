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
#                                Convergence                            ----
#---------------------------------------------------------------------------#

message('Creating: convergance plot')

lik <- read.table(paste0(base_path,'likelihood.txt'), 
                  encoding = 'UTF-8',
                  header = FALSE, 
                  stringsAsFactors = FALSE) %>%
  rename(iter = V1,
         likelihood = V2)

p <- ggplot(lik, aes(x = iter, y = likelihood/1000000000)) +
  geom_line() +
  theme_bw(base_size = 18) +
  geom_point() +
  labs(y = '',
       x = 'Iterations',
       title = '') +
  scale_y_continuous(labels = (scales::comma)) +
  theme_bw()
p
ggsave(paste0(save_path,too_small_threshold_t,"/convergance.png"), 
       plot = p, limitsize = T, 
       height = 10,
       width = 10,
       units = 'cm')


#---------------------------------------------------------------------------#
#                                z-bar plot                            ----
#---------------------------------------------------------------------------#
base_path2 <- paste0('/home/miriam/Downloads/RunSuite',runs[run_nr],'/Run',runs[run_nr],'/Spalias/')
zbar_files <- list.files(base_path2)[list.files(base_path2)%like%'immigration_smoothed_token_ratios']
z_bars <- lapply(paste0(base_path2,zbar_files), function(f) fread(f))
z_bars <- rbindlist(z_bars)
z_bars <- z_bars[, mean(r), by = c('token','year')]
names(z_bars) <- c('token','year','r')
top_z_bar <- z_bars[z_bars[, .I[r == max(r)], by=year]$V1]
top_z_bar <- top_z_bar[order(year),]

library(ggrepel)
exprvec <- expression(bar(z), p==0, p==1)
ggplot(top_z_bar, aes(x = year, y = r, col = token)) +
  labs(y = exprvec,
       x = '') +
  geom_point() +
  geom_text(aes(label=token),hjust=0, 
            vjust=0,
            check_overlap = TRUE) +
  theme_bw(base_size = 20) +
  guides(col = FALSE) + 
  scale_colour_grey() -> z_bar_plot
z_bar_plot

ggsave(paste0(save_path,too_small_threshold_t,"/z_bar_plot.png"), 
       plot = z_bar_plot, limitsize = T, 
       height = 10,
       width = 17,
       units = 'cm')
z_bar_plot





#---------------------------------------------------------------------------#
#                        z-bar (never with seed words)                   ----
#---------------------------------------------------------------------------#
files <- list.files(base_path)[list.files(base_path)%like%'immigration_r_never_with_priors']
pp <- lapply(paste0(base_path,files), function(f) fread(f))
pp <- rbindlist(pp)
pp <- pp[, mean(r), by = c('token')]
names(pp) <- c('token','r')
pp[order(r, decreasing = TRUE),][1:10,]


#---------------------------------------------------------------------------#
#                   agreement human annotator & mdoel                   ----
#---------------------------------------------------------------------------#
# read in known docs
known_docs <- fread(paste0(data_path,'known_doc_topics_indata_extended.csv'))
known_immigration_docs <- known_docs[known_docs$topic%in%'immigration',]

# refine id-var
data[,id2:=gsub(pattern = 'AFTONBLADET_', replacement = '', x = data$id)]
data[,id2:=gsub(pattern = 'EXPRESSEN_', replacement = '', x = data$id2)]
data[,id2:=gsub(pattern = 'SVD_', replacement = '', x = data$id2)]
data[,id2:=gsub(pattern = 'DN_', replacement = '', x = data$id2)]

# correct/incorrect
correct <- data[id2%in%known_immigration_docs$id,]
nrow(correct)/nrow(known_immigration_docs)
fails <- known_immigration_docs[!known_immigration_docs$id%in%data$id2,]
nrow(correct)/nrow(fails)


#---------------------------------------------------------------------------#
#                   make prettier anti-seed word list                 ----
#---------------------------------------------------------------------------#

if(where=='miriam'){
  priors <- c('/Users/mhbodell/Documents/Work/VT19/media_group_threat/bash/priors/cooc_priors_new_v18.txt')
}else if(where=='kb_new'){
  priors <- paste0(git_path,'bash/priors/cooc_priors_new_v18.txt')
}

priors <- fread(priors, header = FALSE, fill = TRUE)
prior_words <- as.vector(as.matrix(priors[36,-1]))
prior_words <- gsub(prior_words, pattern=',', replacement = '')
tmp <- prior_words[prior_words!='']
View(tmp[order(tmp)])


#---------------------------------------------------------------------------#
#                           look at top words                         ----
#---------------------------------------------------------------------------#


topwords <- paste0(base_path,'TopWords.txt')
topwords <- fread(topwords, header = FALSE, fill = TRUE)
topwords <- as.vector(as.matrix(topwords[1,]))
topwords <- gsub(topwords, pattern=',', replacement = '')

tmp <- topwords[topwords!='']
View(tmp[order(tmp)])


#---------------------------------------------------------------------------#
#                          robustness other k                        ----
#---------------------------------------------------------------------------#
source(file = paste0(git_path,'code/paper_ready/utils_funcs.R'))
runs <- c('2021-06-06--18_11_45','2021-06-18--19_18_47','2021-06-24--09_02_42')

for(run_nr in 2:3){
  
  if(where=='kb_new'){
    base_path <- paste0('/home/miriam/Downloads/RunSuite',runs[run_nr],'/Run',runs[run_nr],'/Spalias/')
    save_path <- base_path
    git_path <- '/home/miriam/Documents/git/media_group_threat/'
    data_path <- '/home/miriam/Documents/git/media_group_threat/data/'
  }
  
  # read data
  data <- fread(paste0(base_path,'immigration_ts_doc_multi.csv'))
  nrow(data)
  #data[, r:= get(immigration_topic)/(row_sum*n_draws)]
  gc()
  data <- data[r>=too_small_threshold,]
  nrow(data)
  gc()
  data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
                       ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
                              ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
  table(data$paper)
  
  #----------------------------------------------------------------------------#
  #                 Create Fig. 2a (framing salience)                   ----
  #----------------------------------------------------------------------------#
  # get data of framing salience (date-level)
  df_frame_salience <- get_framing_salience(run = runs[run_nr], data = data)
  
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
  tp_year <- multivariate_bcp[multivariate_bcp$posterior_prob>=0.5,]$year
  # change x-axis labels 1964-1966 --> 1965 to make it look prettier
  tp_year <- c(tp_year[!tp_year%in%c(1964,1966)], 1965)
  tp_year <- tp_year[order(tp_year)]
  
  # create plot
  plot_tp <- plot_turning_points(df_plot = univariate_bcp, turning_points =  tp_year, run = runs[run_nr])
  
  # add lines for turning points in time series of frame salience
  plt_frame_ts + geom_vline(xintercept = tp_year , lty = 'dotted')  -> plt_frame_ts2
  
  # create common legend
  mylegend <- g_legend(plt_frame_ts)
  
  # combine Fig. 2a &  Fig. 2 
  fig2 <- ggpubr::ggarrange(plt_frame_ts2, plot_tp , nrow = 2, ncol = 1, labels = c('A','B'),
                            font.label = list(face = 'plain', size = 11), common.legend = T) 
  fig2
  too_small_threshold_t <-gsub(as.character(too_small_threshold), pattern = '\\.', replacement = '_')
  posthoc_type <- 'minimal'
  if(runs[run_nr]=='2021-06-18--19_18_47'){
    file_name <- paste0(base_path, 'k950_',too_small_threshold_t,'_',posthoc_type,'0_5thres.png')
  }else if(runs[run_nr]=='2021-06-24--09-02-42'){
    file_name <- paste0(base_path, 'k1050_',too_small_threshold_t,'_',posthoc_type,'0_5thres.png')
  }
  ggsave(fig2,  file = file_name, height = 12, width = 15,  units = 'cm')
  
  
}

