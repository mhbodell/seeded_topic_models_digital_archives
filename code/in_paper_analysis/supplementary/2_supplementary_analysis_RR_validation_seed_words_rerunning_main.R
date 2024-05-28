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

#runs <- c('2024-01-11--16_01_43','2024-01-22--00_44_20','2024-01-27--11_21_40')
#run_nr <- length(runs)
#run <- runs[run_nr]
args <- commandArgs(trailingOnly = TRUE)
run <- args[1]
print(run)

where <- 'naiss'

if(where=='naiss'){
  run <-'2024-02-08--09_09_45'
  data_path <- '/proj/efe_et/old_m4m/media_group_threat/data/'
  base_path <- paste0('/proj/efe_et/Runs/RunSuite',run,'/Run',run,'/Spalias/')
  git_path <- '/proj/efe_et/old_m4m/media_group_threat/'
  save_path <- paste0('/proj/efe_et/Runs/RunSuite',run,'/Run',run,'/Spalias/')
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

source(file = paste0(git_path,'code/in_paper_analysis/utils_funcs.R'))


#----------------------------------------------------------------------------#
#          Create Fig 1 (Weekly salience + raw immigration numbs.)        ----
#----------------------------------------------------------------------------#

# read in weekly data
df_plot <- read_format_weekly_data(base_path = base_path)

# read in immigration statistics data (will give warnings - ignore it!)
df_y_inv <- read_format_scb_data(base_path = base_path, git_path = git_path, current_run = run)

# standardized im. stat. data
scalar2 <- round(mean(df_y_inv$count/(df_y_inv$stand_y)))

# order plot data on date
df_plot <- df_plot[order(ymw),]

# create axis labels
casing <- c('1945-01-01','1955-01-01','1965-01-01','1975-01-01','1985-01-01','1995-01-01','2005-01-01','2015-01-01')
casing_labels <- c('1945','1955','1965','1975','1985','1995','2005','2015')

# plot immigration salience in news
# plot immigration salience in news
plt_immigration_salience <- plot_immigration_salience(df_plot = df_plot, data_path = data_path, 
                                                      casing = casing, casing_labels = casing_labels)


plt_immigration_numbs <- plot_immigration_numbs(df_plot = df_y_inv, casing = casing, casing_labels = casing_labels)

# combine plots
salience_scb_plot <- ggpubr::ggarrange(plt_immigration_numbs,
                                       plt_immigration_salience,
                                       nrow =2,
                                       align = c("v"),
                                       labels = c('A','B'),
                                       font.label = list(face = 'plain' )) 
salience_scb_plot

# file name + save
name_thres <- gsub(pattern = '\\.','_',as.character(too_small_threshold))
fn <- paste0(save_path,name_thres,"/salience_scb_small.png")
ggsave(file = fn,    plot = salience_scb_plot, limitsize = T,  height = 12,  width = 15,   units = 'cm')

#----------------------------------------------------------------------------#
#          Calculate correlations between im. numbs & salience           ----
#----------------------------------------------------------------------------#

# over all correlation
if(!'z_1'%in%names(df_y_inv)){
  df_y_inv %>% rename(z_1 = z_0) -> df_y_inv
}
cor.test(df_y_inv$z_1/df_y_inv$N, df_y_inv$count,  method = "spearman",  conf.level = 0.95,  alternative = 'two.sided')

# correlation around 2015 refugee crisis
cor_window <- 5
df_peak2 <- df_y_inv[df_y_inv$year%in%c((2015-cor_window):(2015+cor_window)),]
cor.test(df_peak2$z_1/df_peak2$N, df_peak2$count, method = "spearman", conf.level = 0.95, alternative = 'two.sided')


#----------------------------------------------------------------------------#
#                     Salience in different "eras"                      ----
#----------------------------------------------------------------------------#
period0 <- df_plot[year<=1970,]
period1 <- df_plot[year>=1971 & year<=1985,]
period2 <- df_plot[year>=1986 & year<=1994,]
period3 <- df_plot[year>=1995 & year<=2014,]
period4 <- df_plot[year>=2015 ,]

mean(period0$z_1/period0$N)*100
mean(period1$z_1/period1$N)*100
mean(period2$z_1/period2$N)*100
mean(period3$z_1/period3$N)*100
mean(period4$z_1/period4$N)*100

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

# combine Fig. 2a &  Fig. 2 
fig2 <- ggpubr::ggarrange(plt_frame_ts2, plot_tp , nrow = 2, ncol = 1, labels = c('A','B'),
                          font.label = list(face = 'plain', size = 11), common.legend = T) 
fig2
too_small_threshold_t <-gsub(as.character(too_small_threshold), pattern = '\\.', replacement = '_')
posthoc_type <- 'minimal'
fn2 <- paste0(save_path, too_small_threshold_t,'/cp_eras_',too_small_threshold_t,'_',posthoc_type,'0_5thres.png')
ggsave(fig2,  file = fn2,
       height = 12, width = 15,  units = 'cm')



