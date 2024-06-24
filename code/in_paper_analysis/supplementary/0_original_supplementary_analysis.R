#----------------------------------------------------------------------------#
#                        SUPPLEMENTARY MATERIAL                        ----
#----------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
# load packages
#---------------------------------------------------------------------------#
required_packages <- c(
  "data.table", "dplyr", "ggplot2", "tidyverse", "cowplot", "gtools", 
  "zoo", "knitr", "kableExtra", "RColorBrewer", "xtable", "lubridate", 
  "lmtest", "broom", "ggpubr", "bayesplot", "rstanarm", "gridExtra", 
  "psychometric", "bcp", "ggrepel")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Apply the function to each package
sapply(required_packages, install_if_missing)
#---------------------------------------------------------------------------#
# set up
#---------------------------------------------------------------------------#

# this is the model used in the main analysis
runs <- c('2021-06-06--18_11_45')
run_nr <- 1
where <- 'kb_new'
if(where=='kb_new'){
  # change paths to your local path
  base_path <- paste0('/home/miriam/Documents/git/seeded_topic_models_digital_archives/')
  save_path <- paste0(base_path, 'output/')
  data_path <- paste0(base_path,'data/')
}



#---------------------------------------------------------------------------#
#                                Convergence                            ----
#---------------------------------------------------------------------------#

message('Creating: convergance plot')

lik <- read.table(paste0(data_path,'likelihood.txt'), 
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
# read in saved files of smoothed token ratios (2 draws from the posterior)
zbar_files <- list.files(data_path)[list.files(data_path)%like%'immigration_smoothed_token_ratios']
z_bars <- lapply(paste0(data_path,zbar_files), function(f) fread(f))
z_bars <- rbindlist(z_bars)
z_bars <- z_bars[, mean(r), by = c('token','year')]
names(z_bars) <- c('token','year','r')
# get the tokens with the highest smoothed prob. per year
top_z_bar <- z_bars[z_bars[, .I[r == max(r)], by=year]$V1]
top_z_bar <- top_z_bar[order(year),]
# create pretty label
exprvec <- expression(bar(z), p==0, p==1)
#plot
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


#---------------------------------------------------------------------------#
#                        z-bar (never with seed words)                   ----
#---------------------------------------------------------------------------#
# read in words that never occurs with prior words (4 draws from posterior)
files <- list.files(data_path)[list.files(data_path)%like%'immigration_r_never_with_priors']
pp <- lapply(paste0(data_path,files), function(f) fread(f))
pp <- rbindlist(pp)
# the words with the highest mean immigration prob.
pp <- pp[, mean(r), by = c('token')]
names(pp) <- c('token','r')
# list top 10
pp[order(r, decreasing = TRUE),][1:10,]

#---------------------------------------------------------------------------#
#                   agreement human annotator & mdoel                   ----
#---------------------------------------------------------------------------#
# read in annotated docs
known_docs <- fread(paste0(data_path,'known_doc_topics_indata_extended.csv'))
# select the ones that human annotators say is about immigration
known_immigration_docs <- known_docs[known_docs$topic%in%'immigration',]

# refine id-var
# --> Due to data limitations on Github we provide only a dataset with ids
# that is created on row 132 - 145
# ----> (JUMP TO ROW 147 to read in the small id data) <----
# read in data & select docs based on threshold (NB! local file can not be read)
data <- fread(paste0('/home/miriam/Downloads/RunSuite',runs,'/Run',runs,'/Spalias/immigration_ts_doc_multi.csv'))
data <- data[r>=too_small_threshold,]
# print number of obs included in analysis
nrow(data)
gc()

# add info about which newspaper a text was published in
data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
                     ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
                            ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
# count number of obs. per paper
data.table(data[, c(id)]) -> id_data_small
names(id_data_small) <- 'id'
fwrite(id_data_small, file = paste0(data_path,'original_ids_immigration_rich.csv'))
# -----> READ SMALL ID DATA <------
id_data <- fread(paste0(data_path,'original_ids_immigration_rich.csv'))
id_data[,id2:=gsub(pattern = 'AFTONBLADET_', replacement = '', x = id_data$id)]
id_data[,id2:=gsub(pattern = 'EXPRESSEN_', replacement = '', x = id_data$id2)]
id_data[,id2:=gsub(pattern = 'SVD_', replacement = '', x = id_data$id2)]
id_data[,id2:=gsub(pattern = 'DN_', replacement = '', x = id_data$id2)]

# correct/incorrect
correct <- id_data[id2%in%known_immigration_docs$id,]
nrow(correct)/nrow(known_immigration_docs)
fails <- known_immigration_docs[!known_immigration_docs$id%in%id_data$id2,]
nrow(correct)/nrow(fails)


#---------------------------------------------------------------------------#
#                   make prettier anti-seed word list                 ----
#---------------------------------------------------------------------------#
if(where=='kb_new'){
  priors <- paste0(base_path,'bash/priors/priors_main.txt')
}

# read in priors and sort in alphabetic order
priors <- fread(priors, header = FALSE, fill = TRUE)
prior_words <- as.vector(as.matrix(priors[36,-1]))
prior_words <- gsub(prior_words, pattern=',', replacement = '')
tmp <- prior_words[prior_words!='']
tmp[order(tmp)]

#---------------------------------------------------------------------------#
#                           look at top words                         ----
#---------------------------------------------------------------------------#
# read in the model's "top words"
topwords <- paste0(data_path,'TopWords.txt')
topwords <- fread(topwords, header = FALSE, fill = TRUE)
topwords <- as.vector(as.matrix(topwords[1,]))
topwords <- gsub(topwords, pattern=',', replacement = '')
# print in alphabetic order
tmp <- topwords[topwords!='']
tmp[order(tmp)]

#---------------------------------------------------------------------------#
#                          robustness other k                        ----
#---------------------------------------------------------------------------#
# DUE TO DATA STORAGE LIMITATIONS
source(file = paste0(base_path,'code/in_paper_analysis/utils_funcs.R'))
#1) = original mode, 2) k =950, 3) k = 1050
runs <- c('2021-06-06--18_11_45','2021-06-18--19_18_47','2021-06-24--09_02_42')
save_k <- list()
for(run_nr in 2:3){
# ---> for data storage limtations we only provide the processed data  
# the data read on row 217 in created by the commented out code on line 197--216  
#  if(where=='kb_new'){
#    tmp_base_path <- paste0('/home/miriam/Downloads/RunSuite',runs[run_nr],'/Run',runs[run_nr],'/Spalias/')
#  }
  
  # read data
#  data <- fread(paste0(tmp_base_path,'immigration_ts_doc_multi.csv'))
#  data <- data[r>=too_small_threshold,]
#  nrow(data)
#  gc()
#  data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
#                       ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
#                              ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
#  table(data$paper)
  #----------------------------------------------------------------------------#
  #                 Create Fig. 2a (framing salience)                   ----
  #----------------------------------------------------------------------------#
  # get data of framing salience (date-level)
#  df_frame_salience <- get_framing_salience(current_model = runs[run_nr], data = data)
  #fwrite(df_frame_salience, file = paste0(data_path, 'df_frame_salience_',runs[run_nr],'.csv'))
  # aggregate data to yearly-level
  df_frame_salience <- fread(file = paste0(data_path, 'df_frame_salience_',runs[run_nr],'.csv'))
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
  fig2 <- ggpubr::ggarrange(plt_frame_ts2, plot_tp , nrow = 2, ncol = 1, labels = c('',''),
                            font.label = list(face = 'plain', size = 11), common.legend = T) 
  fig2
  save_k[[run_nr - 1]] <- fig2
  too_small_threshold_t <-gsub(as.character(too_small_threshold), pattern = '\\.', replacement = '_')
  if(runs[run_nr]=='2021-06-18--19_18_47'){
    file_name <- paste0(save_path,too_small_threshold_t, '/fig2_k950.png')
  }else if(runs[run_nr]=='2021-06-24--09_02_42'){
    file_name <- paste0(save_path,too_small_threshold_t, '/fig2_k1050.png')
  }
  ggsave(fig2,  file = file_name, height = 12, width = 15,  units = 'cm')
  
}

# combine plots
plt_diff_k <-  ggpubr::ggarrange( save_k[[1]] , #950
                                   save_k[[2]] ,#1050
                                  labels = c("A",'B'),
                                  nrow = 2, ncol = 1,font.label = list(size = 11,face = "plain"))
plt_diff_k
ggsave(plt_diff_k, 
       file = paste0(save_path, '/0_025/validation_different_k.png'),
       height = 20, width = 15,   units = 'cm')

