#-------------------------
# CREATE LARGER panel

# List of packages to check, install, and load
required_packages <- c(
  "data.table", "dplyr", "ggplot2", "tidyverse", "cowplot",
  "gtools", "zoo", "knitr", "kableExtra", "xtable",
  "lubridate", "lmtest", "broom", "parallel"
)

# Function to check and install packages
install_load_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }
  
  # Load the packages
  lapply(packages, require, character.only = TRUE)
}

# Call the function with the list of required packages
install_load_packages(required_packages)

#-------------
args <- commandArgs(trailingOnly = TRUE)
print(args)
run <- args[1]
where <- args[2]
#run <- '2021-05-26--13_50_02'
#where <- 'm4m'
#---------------
# READ DATA
#---------------

if(where=='kb_new'){
  base_path <- '/home/miriam/Downloads/'
  data_path <- '/home/miriam/Documents/git/media_group_threat/data/'
  save_path <- '/home/miriam/Documents/proj/ws/output/'
  folder_path <- 'SU'
  
}else if(where=='miriam'){
  data_path <- '/Users/mhbodell/Documents/Work/VT19/media_group_threat/data/'
  base_path <- '/Users/mhbodell/Downloads/'
  save_path <- '/Users/mhbodell/Desktop/manual cooccurence/bcp/'
  folder_path <- 'alternative_su'
  
}else if(where=='m4m'){
  data_path <- '/proj/m4m/media_group_threat/data/'
  base_path <- paste0('/proj/m4m/Runs/RunSuite',run,'/Run',run,'/Spalias/')
  save_path <- paste0('/proj/m4m/model_output/Run',run,'/')
  git_path <- '/proj/m4m/media_group_threat/'
  
}else if(where=='KB2'){
  data_path <- '/home/miriam/Documents/data/media_group_threat/'
  base_path <- paste0('/data/miriam/Run',runs[run_nr])
  save_path <- paste0('/data/miriam/Run',runs[run_nr])
  
}else if(where=='efe_et'){
  data_path <- '/proj/efe_et/old_m4m/media_group_threat/data/'
  base_path <- paste0('/proj/efe_et/Runs/RunSuite',run,'/Run',run,'/Spalias/')
  save_path <- paste0('/proj/efe_et/model_output/Run',run,'/')
  git_path <- '/proj/efe_et/old_m4m/media_group_threat/'

}

#df_plot <- fread(paste0(base_path,'weekly_data.csv'))
#df_y <- fread(paste0(base_path,'df_y.csv'))

too_small_threshold <- 0.025
cfg <- list.files(base_path)[which(list.files(base_path)%like%'.cfg')]
cfg <- fread(paste0(base_path,cfg), fill = T)
K <- as.numeric(cfg[V1 == 'topics',]$V3)


im_topic <- paste0('T_',0)
# create categories for r -> used to gain large N in causal fixed effect model
files <- list.files(base_path)
files <- files[files%like%'immigration_ts_doc']
files <- files[files!="immigration_ts_doc_multi.csv"]
n_draws <- length(files)

dats <- fread(paste0(base_path,'/',files)[1])
unique_ids <- dats$id
rowsums_df <- data.frame(id = unique_ids,
                         row_sum = rowSums(dats[,names(dats)%like%'T_', with = F]),
                         date = dats$date)

Ks <- gsub(names(dats)[names(dats)%like%'T_'], pattern ='T_', replacement = '')
Ks <- Ks[order(Ks)]
col_names <- c('id',paste0('T_',Ks))
col_names2 <- paste0('T_',Ks)
dats <- dats[,..col_names]


for(ii in 2:n_draws){
  dats_tmp <- fread(paste0(base_path,'/',files)[ii])
  new_ids <- dats_tmp[which(!dats_tmp$id%in%rowsums_df$id),]
  
  dats_tmp[,date:=NULL]
  dats_tmp <- dats_tmp[,..col_names]
  
  if(nrow(new_ids)>0){
    rowsums_df <- rbind(rowsums_df, 
                        data.frame(id = new_ids$id,
                                   row_sum =  rowSums(new_ids[,names(new_ids)%like%'T_', with = F]),
                                   date = new_ids$date))
    nrow(new_ids)
  }
  
  df_bind <- rbindlist(list(dats, dats_tmp), fill = TRUE)
  df_bind[is.na(df_bind)] <- 0
  dats <- df_bind[, lapply(.SD, sum), by = id]
  print(ii);gc()
}



length(unique(rowsums_df$id))
nrow(dats)
test <- merge(dats,rowsums_df, by = 'id')

test[, r:= (get(im_topic)/(row_sum*n_draws))]


fwrite(test, file = paste0(base_path,'immigration_ts_doc_multi.csv'))

test[, r:= get(im_topic)/(row_sum*n_draws)]
data <- test[r>=too_small_threshold,]
nrow(data)


test_col <- c('T_0','T_1','T_2')
data[,..test_col]
#test<- fread(paste0(base_path,'/',files)[1])
#test[, r:= T_1/rowSums(test[,names(test)%like%'T_', with = F])]
#nrow(test)

gc()
data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
                     ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
                            ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
table(data$paper)



#---------------------------------
# GET POTENTIAL WATERSHEDS    ----
#---------------------------------

df_weekly <- fread(paste0(base_path,'df_weekly.csv'))
df_weekly2 <- fread(paste0(base_path,'df_weekly_z2.csv'))
df_weekly3 <- fread(paste0(base_path,'df_weekly_z3.csv'))

df_weekly <- rbind(df_weekly,df_weekly2,df_weekly3)
fwrite(df_weekly, file = paste0(base_path,'df_weekly_full.csv'))

df_y <- fread(paste0(base_path,'df_y.csv'))
df_y2 <- fread(paste0(base_path,'df_y_z2.csv'))
df_y3 <- fread(paste0(base_path,'df_y_z3.csv'))

df_y <- rbind(df_y, df_y2, df_y3)
df_y <- df_y %>% group_by(year,topic) %>% summarise(z_0=mean(z_0), N = mean(N))
fwrite(df_weekly, file = paste0(base_path,'df_y_full.csv'))


