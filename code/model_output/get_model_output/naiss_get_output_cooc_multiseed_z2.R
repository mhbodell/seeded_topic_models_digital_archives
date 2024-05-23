library(data.table)
library(dplyr)
library(ggplot2)
library(parallel)
library(lubridate)
library(quanteda)
library(zoo)


every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#-------------------
# SET-UP COMPUTER
#-------------------
args <- commandArgs(trailingOnly = TRUE)
run <- args[1]
proj_name <- args[2]
threshold <- as.numeric(args[3])
#run <- '2020-04-27--14_59_45'
#proj_name <- 'm4m'
data_path <- paste0('/proj/',proj_name,'/model_output/z_files/Run',run,'/')
base_path <- paste0('/proj/',proj_name,'/Runs/RunSuite',run,'/Run',run,'/Spalias/')
# update 1/2-24 after NSC --> NAISS
if(proj_name=='efe_et'){
  git_path <- paste0('/proj/',proj_name,'/old_m4m/media_group_threat/')
} else {
  git_path <- paste0('/proj/',proj_name,'/media_group_threat/')
}

#computer <- 'r3'
#base_path <- paste0('/media/',computer,'/Flyttflytt/bash/Runs/RunSuite2020-01-23--18_02_11/Run2020-01-23--18_02_11/Spalias/')

#setwd(base_path)

#--------------------
# GET PRIOR WORDS
#--------------------
cfg <- list.files(base_path)[which(list.files(base_path)%like%'.cfg')]
cfg <- fread(paste0(base_path,cfg), fill = T)
prior_word_file <- cfg[cfg$V1=='topic_prior_filename',]$V3
# update 1/2-24 after NSC --> NAISS
if(proj_name=='efe_et'){
  prior_words_file <-fread(prior_word_file, fill = T)
} else {
  prior_words_file <-fread(paste0('/proj/',proj_name,'/',prior_word_file), fill = T)
}
prior_words <- as.vector(as.matrix(prior_words_file[2,-1]))
prior_words <- gsub(prior_words, pattern=',', replacement = '')
df_priorwords <- data.frame(prior_words = unique(prior_words))
knitr::kable(df_priorwords)


#-------------------------
# GET IMMIGRATION TOPICS
#--------------------------
top_words <- fread(paste0(base_path,list.files(base_path)[list.files(base_path)=='TopWords.txt']), header = F)
relevance_words <- fread(paste0(base_path,list.files(base_path)[list.files(base_path)=='RelevanceWords.txt']), header = F)

# need -1 because index starts at 0
#topicid <- which(apply(top_words,1, function(x) ifelse(length(which(x%like%'invandr'))>0,1,0))==1) - 1

if(run=='2024-01-27--11_21_40'){
  topicids <- list(immigration = c(0),
                   crime = c(1:4), #drugs #coup/robbery
                   education = c(5:6),
                   human_rights = c(7:8,153),
                   eu = c(9),
                   families = c(10:11),
                   labour_market = c(12:14),
                   discrimination = c(15),
                   finance = c(16:17),
                   political_parties = c(18:21),
                   terrorism = c(22:23), # also general "murder"
                   multiculturalism = c(24), 
                   housing = c(25),
                   iraq = c(26),
                   syria = c(27),
                   israel_palestine = c(28),
                   religion = c(29),
                   racism = c(30), # german topic
                   health_care = c(31),
                   swedishness = c(32,280),
                   language = c(33),
                   balkan = c(34))
  
  topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                  'eu', 'families','labour_market','discrimination',
                  'finance','political_parties', 'terrorism', 'multiculturalism', 
                  'housing', 'iraq', 'syria', 'israel_palestine','religion',
                  'racism', 'health_care', 'swedishness', 'language','balkan')
  topicid <- 0
} else if(run=='2024-01-28--10_33_18'){
  topicids <- list(immigration = c(0),
                   crime = c(1:4), #drugs #coup/robbery
                   education = c(5:6),
                   human_rights = c(7:8),
                   eu = c(9),
                   families = c(10:11),
                   labour_market = c(12:14),
                   discrimination = c(15),
                   finance = c(16:17),
                   political_parties = c(18:21),
                   terrorism = c(22:23), # also general "murder"
                   multiculturalism = c(24), 
                   housing = c(25),
                   iraq = c(26),
                   syria = c(27),
                   israel_palestine = c(28),
                   religion = c(29),
                   racism = c(30), # german topic
                   health_care = c(31),
                   swedishness = c(32,418),
                   language = c(33,39),
                   balkan = c(34))
  
  topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                  'eu', 'families','labour_market','discrimination',
                  'finance','political_parties', 'terrorism', 'multiculturalism', 
                  'housing', 'iraq', 'syria', 'israel_palestine','religion',
                  'racism', 'health_care', 'swedishness', 'language','balkan')
  topicid <- 0
} else if(run=='2024-01-28--10_33_25'){
  topicids <- list(immigration = c(0),
                   crime = c(1:4), #drugs #coup/robbery
                   education = c(5:6),
                   human_rights = c(7:8,89),
                   eu = c(9),
                   families = c(10:11),
                   labour_market = c(12:14),
                   discrimination = c(15),
                   finance = c(16:17),
                   political_parties = c(18:21),
                   terrorism = c(22:23), # also general "murder"
                   multiculturalism = c(24), 
                   housing = c(25),
                   iraq = c(26),
                   syria = c(27),
                   israel_palestine = c(28),
                   religion = c(29),
                   racism = c(30), # german topic
                   health_care = c(31),
                   swedishness = c(32,257),
                   language = c(33,312),
                   balkan = c(34))
  
  topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                  'eu', 'families','labour_market','discrimination',
                  'finance','political_parties', 'terrorism', 'multiculturalism', 
                  'housing', 'iraq', 'syria', 'israel_palestine','religion',
                  'racism', 'health_care', 'swedishness', 'language','balkan')
  topicid <- 0
}else if(run=='2024-02-05--12_14_55'){
  topicids <- list(immigration = c(0),
                   crime = c(1:4), 
                   education = c(5:6),
                   human_rights = c(7:8),
                   eu = c(9),
                   families = c(10:11),
                   labour_market = c(12:14),
                   discrimination = c(15),
                   finance = c(16:17),
                   political_parties = c(18:21),
                   terrorism = c(22:23), # also general "murder"
                   multiculturalism = c(24), 
                   housing = c(25),
                   iraq = c(26),
                   syria = c(27),
                   israel_palestine = c(28),
                   religion = c(29),
                   racism = c(30), # german topic
                   health_care = c(31),
                   swedishness = c(32,660),
                   language = c(33,852),
                   balkan = c(34))
  
  topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                  'eu', 'families','labour_market','discrimination',
                  'finance','political_parties', 'terrorism', 'multiculturalism', 
                  'housing', 'iraq', 'syria', 'israel_palestine','religion',
                  'racism', 'health_care', 'swedishness', 'language','balkan')
  topicid <- 0
} else if(run=='2024-02-08--09_09_36'){
  topicids <- list(immigration = c(0),
                   crime = c(1:4), 
                   education = c(5:6),
                   human_rights = c(7:8),
                   eu = c(9),
                   families = c(10:11),
                   labour_market = c(12:14),
                   discrimination = c(15),
                   finance = c(16:17),
                   political_parties = c(18:21),
                   terrorism = c(22:23), # also general "murder"
                   multiculturalism = c(24), 
                   housing = c(25),
                   iraq = c(26),
                   syria = c(27),
                   israel_palestine = c(28),
                   religion = c(29),
                   racism = c(30), # german topic
                   health_care = c(31),
                   swedishness = c(32,757),
                   language = c(33, 468),
                   balkan = c(34))
  
  topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                  'eu', 'families','labour_market','discrimination',
                  'finance','political_parties', 'terrorism', 'multiculturalism', 
                  'housing', 'iraq', 'syria', 'israel_palestine','religion',
                  'racism', 'health_care', 'swedishness', 'language','balkan')
  topicid <- 0
}  else if(run=='2024-02-08--09_09_45'){
  topicids <- list(immigration = c(0),
                   crime = c(1:4), 
                   education = c(5:6),
                   human_rights = c(7:8),
                   eu = c(9),
                   families = c(10:11),
                   labour_market = c(12:14),
                   discrimination = c(15),
                   finance = c(16:17),
                   political_parties = c(18:21),
                   terrorism = c(22:23), 
                   multiculturalism = c(24), 
                   housing = c(25),
                   iraq = c(26),
                   syria = c(27),
                   israel_palestine = c(28),
                   religion = c(29),
                   racism = c(30), 
                   health_care = c(31),
                   swedishness = c(32),
                   language = c(33, 252),
                   balkan = c(34))
  
  topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                  'eu', 'families','labour_market','discrimination',
                  'finance','political_parties', 'terrorism', 'multiculturalism', 
                  'housing', 'iraq', 'syria', 'israel_palestine','religion',
                  'racism', 'health_care', 'swedishness', 'language','balkan')
  topicid <- 0
}







message(paste0('Number of topics: ',length(topicnames)))


#----------------------------------
# REAL RUN
#-----------------------------------

files <- paste0(data_path,list.files(data_path))
base_files <- list.files(base_path)

print(head(files))

find_topic_blocks <- function(data_path, 
                              topics){
  data <- fread(data_path)
  print(data)
  contain_topic <- unique(data[z%in%topics,]$id)
  
  
  return(data[id%in%contain_topic])
}


for(ti in 1){
  #ti <- 2
  print(ti)
  cores <- detectCores() - 2
  cl <- makeCluster(cores)
  var_func_list <- c('find_topic_blocks','topicids','ti') 
  clusterEvalQ(cl, {library(data.table)})
  clusterExport(cl = cl, varlist = var_func_list, envir = environment())
  gc()
  #print(files[1:3])
  
  system.time(dt <- parLapply(cl = cl,
                              X = files,
                              fun = function(x) try(find_topic_blocks(data_path = x,
                                                                      topics = topicids[[ti]]))))
  
  stopCluster(cl)
  gc()
  print(dt[[1]])
  dt <- rbindlist(dt, fill = T)
  #fwrite(dt, paste0(base_path, topicnames[ti],'_dt.csv'))
  
  #---------------------------------
  # STUDY IMMIGRATION TEXTBLOCKS
  #---------------------------------
  
  #--------------------------------
  # PLOT IMMIGRATION TOKEN PLOTS
  #--------------------------------
  
  dt[, year := lubridate::year(date)]
  # Find the token that most often is estimated to be in an immigration topic per year
  
  
  if(!any(base_files%in%'r_never_with_priors_z2.csv')){
    message('Create: r_never_with_priors_z2.csv')
    pw <- as.vector(as.matrix(prior_words_file[topicids[[ti]]+1,-1]))
    pw <- unique(pw)[which(pw!='')]
    pw <- gsub(pw, pattern=',', replacement = '')
    dt %>%
      group_by(id) %>%
      summarise(include_prior_words = sum(ifelse(token%in%pw,1,0))) %>%
      filter(include_prior_words>0) %>%
      pull(id) -> id_include_prior_words
    
    
    dt %>%
      filter(id %in%id_include_prior_words) -> prior_df
    
    prior_df <- as.data.table(prior_df)
    prior_df <- prior_df[,  paste0(token, collapse = ' '), by = c('id','date')]
    fcm(prior_df$V1) -> prior_fcm
    colnames(prior_fcm) -> cooccurence_prior_words
    
    
    dt %>% 
      filter(z %in% topicids[[ti]]) %>%
      filter(!token%in%cooccurence_prior_words) %>% 
      group_by(token,z) %>%
      summarise(z_n = n()) -> token_z_n
    
    non_prior_words <- token_z_n$token[order(token_z_n$z_n, decreasing = TRUE)][1:(0.33*nrow(token_z_n))]
    
    find_token_blocks <- function(data_path, 
                                  words){
      data <- fread(data_path)
      
      return(data[token%in%words,])
    }
    #top_z_tok <- dt %>% group_by(token) %>% summarise(z_1 = sum(ifelse(z%in%topicid,1,0)),
    #                                                  N = n())
    
    cores <- detectCores() - 2
    cl <- makeCluster(cores)
    var_func_list <- c('find_token_blocks','non_prior_words') 
    clusterEvalQ(cl, {library(data.table)})
    clusterExport(cl = cl, varlist = var_func_list, envir = environment())
    gc()
    
    system.time(token_N <- parLapply(cl = cl,
                                     X = files,
                                     fun = function(x) try(find_token_blocks(data_path = x,
                                                                             words = non_prior_words))))
    stopCluster(cl)
    gc()
    token_N <- rbindlist(token_N)
    token_N <- token_N[,.N, by = 'token']
    
    beta <- as.numeric(cfg[V1 == 'beta',]$V3)
    K <- as.numeric(cfg[V1 == 'topics',]$V3)
    
    merge(token_N, token_z_n, by = 'token') %>% 
      group_by(token,z) %>% 
      summarise(z_N = sum(z_n), 
                N = sum(N)) %>%
      mutate(r = (z_N + beta)/(N+K*beta)) %>%
      arrange(desc(r)) %>%
      data.table() -> smoothed_words_to_check
    
    # words that often are estimated as "immigration" but never occur together with a prior word (ratio)
    #top_z_tok %>% filter(z_1>0) %>% filter(!token%in%cooccurence_prior_words) %>% mutate(r = z_1/N) %>% arrange(desc(r)) -> words_to_check
    #data.table(words_to_check)[which(r>0.7),]
    fwrite(smoothed_words_to_check, paste0(base_path,topicnames[ti],'_r_never_with_priors_z2.csv'))
    
    
  } else{
    message('Already exists: r_never_with_priors_z2.csv')
  }
  
  
  #---------------------
  # GET CO-OCCURENCES
  #--------------------
  #dt[, year := lubridate::year(date)]
  
  
  # save
  if(topicnames[ti]=='immigration'){
    dt[, month := lubridate::month(date)]
    dt[, week := lubridate::week(date)]
    dt[, ymw := paste0(year,'-',month,'-',week)]
    message('Creating: ts_doc_z2.csv')
    
    # create time index
    data.table(dt) %>% group_by(year, month, week, ymw, date) %>% summarise(N=n())  -> ind
    #data.table(df_immigration) %>% group_by(paper,date, ymw) %>% summarise(N=n())  -> ind
    ind %>% ungroup %>% mutate(date = as.Date(date)) %>% arrange(date) %>% mutate(ind = 1:nrow(ind)) -> ind
    ind %>% group_by(ymw) %>% summarise(ind = min(ind)) %>% arrange(ind) -> ind
    #ind %>% group_by(ymw,paper) %>% summarise(ind = min(ind)) %>% arrange(ind) -> ind
    nrow(ind) -> nr
    ind %>% mutate(ind = 1:nr) -> ind
    
    # create wide format - per textblock
    data.table(dt)[,.N, by = c('id','z')] -> ts_immigration_doc
    reshape(data.frame(ts_immigration_doc), idvar = c('id'), timevar = 'z', direction = 'wide') -> ts_immigration_doc
    names(ts_immigration_doc) <- gsub(names(ts_immigration_doc), pattern = 'N.', replacement = 'T_')
    
    # replace NA with 0
    ts_immigration_doc[is.na(ts_immigration_doc)] <- 0
    
    # add dates
    cccc <- c('id','date')
    dates <- unique(dt[,..cccc])
    ts_immigration_doc <- merge(ts_immigration_doc, dates)
    
    fwrite(ts_immigration_doc, paste0(base_path,topicnames[ti],'_ts_doc_z2.csv'))
  } else {
    message(topicnames[ti])
   }
} 


#----------------------------------
# CORRELATION IMMIGRATION  SCB
#----------------------------------

if(!any('df_y_z2.csv'%in%base_files)){
  message('Creating: df_y_z2.csv')
  yearly_summary <- function(data_path, 
                             topics){
    data <- fread(data_path)
    data[, year := lubridate::year(date)]
    
    df_y <- list() 
    #for(i in 1:length(topics)){
    data %>%
      group_by(year) %>%
      summarise(z_0 = sum(ifelse(z%in%topics,1,0)),
                N = n(),
                topic = 'immigration') -> tmp
    df_y <- tmp
    
    #}
    
    #df_y <- rbindlist(df_y, fill = T)
    return(df_y)
  }
  
  
  cores <- detectCores() - 2
  cl <- makeCluster(cores)
  var_func_list <- c('yearly_summary','topicid') 
  clusterEvalQ(cl, {library(data.table);library(lubridate); library(dplyr)})
  clusterExport(cl = cl, varlist = var_func_list, envir = environment())
  gc()
  
  system.time(df_y <- parLapply(cl = cl,
                                X = files,
                                fun = function(x) try(yearly_summary(data_path = x,
                                                                     topics = topicid))))
  
  stopCluster(cl)
  gc()
  df_y <- rbindlist(df_y, fill = T)
  df_y %>% group_by(year,topic) %>% summarise(z_0 = sum(z_0), N = sum(N)) -> df_y
  fwrite(df_y, paste0(base_path, 'df_y_z2.csv'))
  
} else {
  message('Already exists: df_y_z2.csv')
  df_y <- fread(paste0(base_path, 'df_y_z2.csv'))
}

library(synchrony)
scb_path <- paste0('/proj/',proj_name,'/old_m4m/media_group_threat/data/')

# data for immigrants
inv <- read.csv(paste0(scb_path, 'invandring1.csv'),
                sep = ';',
                encoding = 'UTF-8', 
                header = FALSE)

names(inv) <- inv[1,]
inv <- inv[-1,]
inv %>%
  tidyr::gather(., key = 'year', value = 'count') %>%
  mutate(type = 'Immigrants',
         year = as.numeric(year)) -> inv

#combine with topic estimations
df_y_inv <- dplyr::left_join(x=df_y[which(df_y$year>=1945 & df_y$year<2019),], y = inv[which(inv$year>=1945 & inv$year<2019),], by="year")


# plot the raw counts
scalar <- round(mean(df_y_inv$count/df_y_inv$z_0))
pscb_raw <-  ggplot(df_y_inv) +
  geom_point(aes(x = year, y = count/scalar, color = 'grey')) +
  geom_line(aes(x = year, y = count/scalar, group = 1, color = 'grey'), size = 1) + 
  geom_point(aes(x = year, y = z_0, color = 'black')) +
  geom_line(aes(x = year, y = z_0, group = 1, color = 'black'), size = 1, linetype = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1940,2020,10)) +
  labs(color = '',
       x = 'Year') +
  scale_color_manual(labels = c('Immigration Topic','Immigration'),
                     values = c('black','grey')) +
  scale_y_continuous(name = 'Count immigration token',
                     sec.axis = sec_axis(~.*scalar, name = "Number of Immigrants (SCB)")) +
  theme_bw(base_size = 34) +
  theme(legend.position="bottom") +
  facet_wrap(~topic)
pscb_raw
ggsave(paste0(base_path,"scb_yearly_raw_z2.png"), plot = pscb_raw, limitsize = T, height = 350, width = 500, units = 'mm')


df_y_inv %>% mutate(stand_y = z_0/N) -> df_y_inv

# plot standardized rasults
scalar2 <- round(mean(df_y_inv$count/df_y_inv$stand_y))
pscb <- ggplot(df_y_inv) +
  geom_point(aes(x = year, y = count/scalar2, color = 'grey')) +
  geom_line(aes(x = year, y = count/scalar2, group = 1, color = 'grey'), size = 1) + 
  geom_point(aes(x = year, y = stand_y, color = 'black')) +
  geom_line(aes(x = year, y = stand_y, group = 1, color = 'black'), size = 1, linetype = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1940,2020,10)) +
  labs(color = '',
       x = 'Year') +
  scale_color_manual(labels = c('Immigration Topic','Immigration'),
                     values = c('black','grey')) +
  scale_y_continuous(name = 'Immigration tokens/Total tokens',
                     sec.axis = sec_axis(~.*scalar2, name = "Number of Immigrants (SCB)")) +
  theme_bw(base_size = 34) +
  theme(legend.position="bottom") +
  facet_wrap(~topic)
pscb
ggsave(paste0(base_path,"scb_yearly_standardized_z2.png"), plot = pscb, limitsize = T, height = 350, width = 500, units = 'mm')

# calcualte correaltion raw number of topic indicators and standardised with number of tokens in textblock
rho_stand <- cor(df_y_inv$count/scalar2, df_y_inv$stand_y, method = 'spearman')
rho_raw <- cor(df_y_inv$count/scalar, df_y_inv$z_0, method = 'spearman')
message(paste0('Spearman: ', rho_stand))




#----------------
# WEEKLY PLOTS
#---------------

# plot ratio of number of immigration topic of all tokens -> salience per week
if(!any(base_files%like%'df_weekly_z2.csv')){
  message('Creating: df_weekly_z2.csv')
  weekly_summary <- function(data_path, 
                             topics){
    data <- fread(data_path)
    data[, year := lubridate::year(date)]
    data[,month := lubridate::month(date)]
    data[,week := lubridate::week(date)]
    data[,ymw := paste0(year,'-',month,'-',week)]
    
    df_plot <- list()
    for(i in 1:length(topics)){
      data%>%
        # mutate(date = as.Date(date)) %>%
        group_by(year,month,week,ymw, date) %>%
        summarise(z_1 = sum(ifelse(z%in%topics[i],1,0)),
                  N = n(),
                  topic = topics[i]) -> tmp
      tmp$date <- as.Date(tmp$date)
      df_plot[[i]] <- tmp
    }
    
    df_plot <- rbindlist(df_plot)
    return(df_plot)
  }
  
  
  cores <- detectCores() - 2
  cl <- makeCluster(cores)
  var_func_list <- c('weekly_summary','topicid') 
  clusterEvalQ(cl, {library(data.table);library(dplyr); library(zoo)})
  clusterExport(cl = cl, varlist = var_func_list, envir = environment())
  gc()
  
  system.time(df_weekly <- parLapply(cl = cl,
                                     X = files,
                                     fun = function(x) try(weekly_summary(data_path = x,
                                                                          topics = topicid))))
  
  stopCluster(cl)
  gc()
  df_weekly <- rbindlist(df_weekly, fill = T)
  df_weekly %>% group_by(year,month,week,ymw, date, topic) %>% summarise(z_1 = sum(z_1), N = sum(N)) -> df_weekly
  fwrite(df_weekly, paste0(base_path, 'df_weekly_z2.csv'))
  
} else {
  df_weekly <- fread(paste0(base_path, 'df_weekly_z2.csv'))
}


df_weekly %>% ungroup() %>% arrange(date) %>% mutate(ind = 1:nrow(df_weekly)) -> df_plot2
df_plot2 <- data.table(df_plot2)
df_plot2[,month := ifelse(nchar(month)==1,paste0('0',month),month)]
df_plot2[,week := ifelse(nchar(week)==1,paste0('0',week),week)]
df_plot2[,ymw := paste0(year,'-',month,'-',week)]


# calculate outliers (yearly average and sd)
df_plot2 %>%
  group_by(year,month,week,ymw) %>%
  summarise(z_1 = sum(z_1),
            N = sum(N)) -> weekly_salience
weekly_salience %>%
  group_by(year) %>%
  arrange(desc(z_1/N)) %>%
  slice(1:10) -> top_yearly_weeks


weekly_salience %>%
  ungroup() %>%
  arrange(desc(z_1/N)) %>%
  slice(1:round(nrow(weekly_salience)*0.1)) -> top_total 
outliers <- unique(bind_rows(top_yearly_weeks, top_total))


events_df <- readxl::read_xlsx(paste0(git_path,'/data/df_watershed.xlsx'))
events_df$start_date <- as.Date(events_df$start_date)
events_df <- data.table(events_df)
events_df[,y := lubridate::year(events_df$start_date)]
events_df[,m := lubridate::month(events_df$start_date)]
events_df[,m := ifelse(nchar(m)==1,paste0('0',m),m)]
events_df[,w := lubridate::week(events_df$start_date)]
events_df[,w := ifelse(nchar(w)==1,paste0('0',w),w)]
events_df[,w2 := ifelse(nchar(as.numeric(w)+1)==1,paste0('0',as.numeric(w)+1),as.numeric(w)+1)]
events_df[,ymw := paste0(y,'-',m,'-',w)]
events_df[,ymw2 := paste0(y,'-',m,'-',w2)]
events_df_tmp2 <- events_df[events_df$ymw%in%outliers$ymw | events_df$ymw2%in%outliers$ymw ,]
events_df_tmp2
#events_df_tmp2[y>2010 & y <2015,]

df_plot2 <- data.table(df_plot2)
df_plot2[,month := ifelse(nchar(month)==1,paste0('0',month),month)]
df_plot2[,week := ifelse(nchar(week)==1,paste0('0',week),week)]
df_plot2[,ymw := paste0(year,'-',month,'-',week)]

df_plot2 %>%
  group_by(year,month,week,ymw) %>%
  summarise(z_1 = sum(z_1),
            N = sum(N))  %>%
  data.table()-> df_plot3

df_plot3[,outlier_col := ifelse(ymw%in%outliers$ymw,'1','0')]
#df_plot2[, outlier_col := ifelse(r>=2*yearly_mean,'black','lightgrey')]
df_plot3[, outlier_col2 := ifelse(ymw%in%events_df_tmp2$ymw,'2',outlier_col)]
df_plot3[, outlier_size := as.factor(ifelse(outlier_col2=='2',5,4))]


df_plot3[outlier_col2 =='2',]
pw <- df_plot3 %>%
  ggplot(aes(x = ymw, y = z_1/N)) +
  geom_point(aes(fill = outlier_col2, size = outlier_size),col="black",pch=21) +
  geom_smooth(method = 'loess')  + 
  geom_line(data = df_y, aes(x = paste0(year,'-05-19'), y = z_0/N, group =1, col = 'Yearly'),size = 1) +
  theme_bw() +
  scale_x_discrete(breaks = every_nth(n = 1000)) +
  labs(x = 'Year-Month-Week',
       y = 'Weekly ratio of Immigration topic',
       color = 'Time series') + 
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Yearly ratio of Immigration topic")) +
  scale_fill_manual(values = c( "#999999","#000000","#E69F00", "#56B4E9"),
                    labels = c('Weekly','High Salience','High Salience + Known Event','Yearly')) +
  theme_bw(base_size = 34) +
  theme(legend.position = c(0.25,0.8)) +
  #  annotate('rect', xmin = '1958-01-01', xmax = '1964-12-31', ymin = 0, ymax = Inf,
  #           alpha = 0.2,
  #           fill= "green") +
  #  annotate('rect', xmin = "1973-01-01", xmax = "1975-12-31", ymin = 0, ymax = Inf,
  #           alpha = 0.2,
  #           fill= "green") +
  #  annotate('rect', xmin = '1980-01-01', xmax = '1988-12-31', ymin = 0, ymax = Inf,
  #           alpha = 0.2,
  #           fill= "green") +
  #  annotate('rect', xmin = "1999-01-01", xmax = "2002-12-31", ymin = 0, ymax = Inf,
  #           alpha = 0.2,
#           fill= "green") +
#  annotate('rect', xmin = '2011-01-01', xmax = '2015-12-31', ymin = 0, ymax = Inf,
#           alpha = 0.2,
#           fill= "green") +
guides(size = FALSE,
       fill = FALSE,
       col = FALSE,
       shape = FALSE)
pw


# using xrank instead
ggsave(paste0(base_path,"weekly_yearly_tot_outliers_z2.png"), plot = pw, limitsize = T, height = 350, width = 500, units = 'mm')


pw_early <- df_plot3 %>%
  filter(year < 1990) %>%
  ggplot(aes(x = ymw, y = z_1/N)) +
  geom_point(aes(fill = outlier_col2, size = outlier_size),col="black",pch=21) +
  geom_smooth(method = 'loess')  + 
  geom_line(data = df_y, aes(x = paste0(year,'-05-19'), y = z_0/N, group =1, col = 'Yearly'),size = 1) +
  theme_bw() +
  scale_x_discrete(breaks = every_nth(n = 1000)) +
  labs(x = 'Year-Month-Week',
       y = 'Weekly ratio of Immigration topic',
       color = 'Time series') + 
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Yearly ratio of Immigration topic")) +
  scale_fill_manual(values = c( "#999999","#000000","#E69F00", "#56B4E9"),
                    labels = c('Weekly','High Salience','High Salience + Known Event','Yearly')) +
  theme_bw(base_size = 34) +
  theme(legend.position = c(0.25,0.8)) +
  guides(size = FALSE,
         fill = FALSE,
         col = FALSE,
         shape = FALSE)
pw_early
ggsave(paste0(base_path,"weekly_yearly_outliers_pre90_z2.png"), plot = pw_early, limitsize = T, height = 350, width = 500, units = 'mm')
fwrite(df_plot3, file = paste0(base_path,'weekly_data_z2.csv'), row.names = F)




if(!any(base_files%like%'immigration_dt_z2.csv')){
  message('Creating: immigration_dt_z2.csv')
  
  find_topic_blocks <- function(data_path, 
                                topics){
    data <- fread(data_path)
    contain_topic <- unique(data[z%in%topics,]$id)
    
    
    return(data[id%in%contain_topic])
  }
  
  
  cores <- detectCores() - 5
  cl <- makeCluster(cores)
  #topicid <- 1 
  var_func_list <- c('find_topic_blocks','topicid') 
  clusterEvalQ(cl, {library(data.table)})
  clusterExport(cl = cl, varlist = var_func_list, envir = environment())
  gc()
  
  system.time(dt <- parLapply(cl = cl,
                              X = files,
                              fun = function(x) try(find_topic_blocks(data_path = x,
                                                                      topics = topicid))))
  
  stopCluster(cl)
  gc()
  dt <- rbindlist(dt, fill = T)
  fwrite(dt, paste0(base_path, 'immigration_dt_z2.csv'))
  
} 

