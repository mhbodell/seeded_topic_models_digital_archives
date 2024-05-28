#----------------------------------------------------------------------------#
#                                 MAIN                                   ----
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



#----------------------------------------------------------------------------#
#          Create Fig 1 (Weekly salience + raw immigration numbs.)        ----
#----------------------------------------------------------------------------#

# read in weekly data
df_plot <- read_format_weekly_data(data_path = data_path)

# read in immigration statistics data (will give warnings - ignore it!)
df_y_inv <- read_format_scb_data(data_path = data_path)

# standardized im. stat. data
scalar2 <- round(mean(df_y_inv$count/(df_y_inv$stand_y)))

# order plot data on date
df_plot <- df_plot[order(ymw),]

# create axis labels
casing <- c('1945-01-01','1955-01-01','1965-01-01','1975-01-01','1985-01-01','1995-01-01','2005-01-01','2015-01-01')
casing_labels <- c('1945','1955','1965','1975','1985','1995','2005','2015')

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
# set immigration rich threshold level
too_small_threshold <- 0.025
name_thres <- gsub(pattern = '\\.','_',as.character(too_small_threshold))
fn <- paste0(save_path,name_thres,"/fig1.png")
ggsave(file = fn,    plot = salience_scb_plot, limitsize = T,  height = 12,  
       width = 15,   units = 'cm')

#----------------------------------------------------------------------------#
#          Calculate correlations between im. numbs & salience           ----
#----------------------------------------------------------------------------#

# over all correlation
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

# --> Due to data limitations on Github we provide only the processed dataset
# that is created on row 131 - 146
# ----> (JUMP TO ROW 148 to read in the processed data) <----
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
table(data$paper)

# get data of framing salience (date-level)
df_frame_salience <- get_framing_salience(current_model = runs[run_nr], data = data)
fwrite(df_frame_salience, file = paste0(data_path, 'df_frame_salience.csv'))
# -----> READ PROCESSED DATA <------
df_frame_salience <- fread(paste0(data_path, 'df_frame_salience.csv'))

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
fig2 <- ggpubr::ggarrange(plt_frame_ts2, plot_tp , nrow = 2, ncol = 1, labels = c('A','B'),
                         font.label = list(face = 'plain', size = 11), common.legend = T) 
fig2
too_small_threshold_t <-gsub(as.character(too_small_threshold), pattern = '\\.', replacement = '_')
posthoc_type <- 'minimal'
ggsave(fig2,  file = paste0(save_path, too_small_threshold_t,'/fig2.png'),
       height = 12, width = 15,  units = 'cm')




