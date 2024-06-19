

#----------------------------------------------------------------------------#
#                        help func when plotting                         ----
#----------------------------------------------------------------------------#

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#----------------------------------------------------------------------------#
#                    Identify high immigration salience weeks            ----
#----------------------------------------------------------------------------#

identify_high_salience <- function(base_path = base_path){
  # read in data and ad id + date info
  df_weekly <- fread(paste0(base_path,'df_weekly_full.csv'))
  df_weekly %>% ungroup() %>% arrange(date) %>% mutate(ind = 1:nrow(df_weekly)) %>% data.table()-> df_plot2
  df_plot2[,month := ifelse(nchar(month)==1,paste0('0',month),month)]
  df_plot2[,week := ifelse(nchar(week)==1,paste0('0',week),week)]
  df_plot2[,ymw := paste0(year,'-',month,'-',week)]
  
  # find weekly salience
  df_plot2 %>%
    group_by(year,month,week,ymw) %>%
    summarise(z_1 = sum(z_1),
              N = sum(N)) -> weekly_salience
  
  # find top 5 weeks per year
  weekly_salience %>%
    group_by(year) %>%
    arrange(desc(z_1/N)) %>%
    dplyr::slice(1:5) -> top_yearly_weeks
  
  # find top 5% weeks in total (so that high salience years are not "punished")
  weekly_salience %>%
    ungroup() %>%
    arrange(desc(z_1/N)) %>%
    dplyr::slice(1:round(nrow(weekly_salience)*0.05)) -> top_total 
  
  # combine & save to these "outliers"
  outliers <- unique(bind_rows(top_yearly_weeks, top_total))
  return(outliers)
}


#----------------------------------------------------------------------------#
#                       Read and format event data                     ----
#----------------------------------------------------------------------------#
read_format_event_data <- function(data_path = data_path){
  # read in data
  events_df <- readxl::read_xlsx(paste0(data_path,'df_watershed.xlsx'))
  #format date column
  events_df$start_date <- as.Date(events_df$start_date)
  #change groups names
  events_df$group[events_df$event_names%like%'sd_'] <-'event'
  events_df <- data.table(events_df)
  # create new date variables
  events_df[,y := lubridate::year(events_df$start_date)]
  events_df[,m := lubridate::month(events_df$start_date)]
  events_df[,m := ifelse(nchar(m)==1,paste0('0',m),m)]
  events_df[,w := lubridate::week(events_df$start_date)]
  events_df[,w := ifelse(nchar(w)==1,paste0('0',w),w)]
  events_df[,w2 := ifelse(nchar(as.numeric(w)+1)==1,paste0('0',as.numeric(w)+1),as.numeric(w)+1)]
  # add one varibale for year-month-week cobinations
  events_df[,ymw := paste0(y,'-',m,'-',w)]
  # add one varibale for year-month-week+1 cobinations
  # week+1 to also capture events where spills over into next week (e.g. happened on Sunday)
  events_df[,ymw2 := paste0(y,'-',m,'-',w2)]
  
  return(events_df)
}


#----------------------------------------------------------------------------#
#                 Read and format weekly salience data                   ----
#----------------------------------------------------------------------------#

read_format_weekly_data <- function(data_path = data_path){
  df_weekly <- fread(paste0(data_path, 'df_weekly_full.csv'))
  df_weekly %>% ungroup() %>% arrange(date) %>% mutate(ind = 1:nrow(df_weekly)) -> df_plot2
  # create "pretty labels"
  df_plot2 <- data.table(df_plot2)
  df_plot2[,month := ifelse(nchar(month)==1,paste0('0',month),month)]
  df_plot2[,week := ifelse(nchar(week)==1,paste0('0',week),week)]
  df_plot2[,ymw := paste0(year,'-',month,'-',week)]
  
  # summarise salience per week
  df_plot2 %>%
    group_by(year,month,week,ymw) %>%
    summarise(z_1 = sum(z_1),
              N = sum(N))  %>%
    data.table()-> df_plot3
  gc()
  return(df_plot3)
}

#----------------------------------------------------------------------------#
#                   Read and format raw im. data (SCB)                  ----
#----------------------------------------------------------------------------#

read_format_scb_data <- function(data_path = data_path, current_run = runs[run_nr]){
  df_y <- fread(paste0(data_path,'df_y_full.csv'))
  if(current_run%in%c('2021-06-24--01_15_10','2024-01-11--16_01_43','2024-01-22--00_44_20','2024-01-28--10_33_18','2024-02-03--09_18_24')){
    df_y <- df_y %>% group_by(year,topic) %>%summarise(z_0 = mean(z_0),N=mean(N)) 
  }else{
    df_y <- df_y %>% group_by(year,topic) %>%summarise(z_1 = mean(z_1),N=mean(N)) 
  }
  inv <- read.csv(paste0(data_path, 'invandring1.csv'),
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
  
  # take the standardized ratio of tokens
  if(current_run%in%c('2021-06-24--01_15_10','2024-01-11--16_01_43','2024-01-22--00_44_20','2024-01-28--10_33_18','2024-02-03--09_18_24')){
    df_y_inv %>% mutate(stand_y = (z_0/N)) -> df_y_inv
  }else{
    df_y_inv %>% mutate(stand_y = (z_1/N)) -> df_y_inv
  }
  
  return(df_y_inv)
}

#----------------------------------------------------------------------------#
#                      Plot immgiration salience                       ----
#----------------------------------------------------------------------------#

plot_immigration_salience <-  function(df_plot = df_plot, data_path = data_path, casing = casing, casing_labels = casing_labels, current_run = runs[run_nr]){
  
  df_y <- fread(paste0(data_path,'df_y_full.csv'))
  if(current_run%in%c('2021-06-24--01_15_10','2024-01-11--16_01_43','2024-01-22--00_44_20','2024-01-28--10_33_18','2024-02-03--09_18_24')){
    df_y <- df_y %>% group_by(year,topic) %>%summarise(z_1 = mean(z_0),N=mean(N)) 
  }else{
    df_y <- df_y %>% group_by(year,topic) %>%summarise(z_1 = mean(z_1),N=mean(N)) 
  }
  
  
  pw <- df_plot %>%
    #filter(highsal_col2!='2') %>%
    ggplot(aes(x = ymw, y = (z_1/N)*100)) +
    geom_point(aes(fill = '0',col='0'), size = 0.3,pch=21) +
    #geom_point(data = df_plot[highsal_col2=='2',], aes(fill = '2'), size = 1, pch = 21, col = 'black') +
    geom_smooth(method = 'loess')  + 
    geom_line(data = df_y, aes(x = paste0(year,'-08-31'), y = (z_1/N)*100), col = 'blue3', group =1,size = 1) +
    scale_x_discrete(breaks = casing,
                     labels = casing_labels,
                     expand = c(0, 0))  +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1)),
                       breaks = seq(0,1.25, by = 0.25),
                       labels = as.character(seq(0,1.25, by = 0.25))) +
    labs(x = '',
         y = 'Immigration salience',
         color = 'Time series') + 
    scale_fill_manual(values = c( "#999999","#000000","#E69F00"),
                      labels = c('0','1','2')) +
    scale_color_manual(values = c( "#999999","#000000","#E69F00"),
                       labels = c('0','1','2')) +
    theme_bw(base_size = 22) +
    guides(size = FALSE,
           fill = FALSE,
           col = FALSE,
           shape = FALSE) 
  
  weekly_plot_mod <- pw +
    #  h2 + 
    theme(plot.margin = unit(c(-0.3,0.3,0.3,0.3), "lines")) +
    theme_bw(base_size = 13) +
    theme(#axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x=element_blank(),
      axis.title.y= element_text(size = 10))+
    #     coord_equal() +
    labs(y = 'Immigration salience')
  
  return(weekly_plot_mod)
  
}

#----------------------------------------------------------------------------#
#                         Plot SCB raw im. numbs                       ----
#----------------------------------------------------------------------------#

plot_immigration_numbs <-  function(df_plot = df_y_inv,  casing = casing, casing_labels = casing_labels){
  ggplot(df_plot, aes(x = paste0(year,'-01-01'), group = 1, y = count/1000)) +
    geom_line(size = 1) +
    theme(aspect.ratio=1) +
    theme_bw(base_size = 22) +
    labs(x = '',
         y = 'Annual # of immigrants (in thousands)') +
    scale_x_discrete(breaks = casing,
                     labels = casing_labels,
                     expand = c(0, 0))  +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1)),
                       breaks = c(50,100,150)) -> immigration_plot
  immigration_plot
  
  
  imm_plot <- immigration_plot + theme_bw(base_size = 13) + 
    # theme(plot.margin = unit(c(0.3,0.3,-0.3,0.3), "lines")) + 
    theme(axis.text.x=element_blank(),
          axis.title.y = element_text(size = 10)) +
    #    coord_equal() +
    labs(y ='Annual # immigrants')
  return(imm_plot)
}
  





#----------------------------------------------------------------------------#
#                 Format & extract framing salience                    ----
#----------------------------------------------------------------------------#

get_framing_salience <- function(current_model ="2021-06-06--18_11_45", data = data) {
  
  # which colums are seeded with which frames (depend on model run, due to some post-hoc fixes -- see supplementary material)
  if(current_model =='2021-06-06--18_11_45'){
    agg_topics <-  list(tmp = list(immigration = c(0),
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
                                   racism = c(30,707), # fix post-hoc
                                   health_care = c(31),
                                   swedishness = c(785), # fix post-hoc
                                   language = c(33),
                                   balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
    
  }else if(current_model =='2021-06-18--19_18_47'){
    agg_topics <-  list(tmp=list(immigration = c(0),
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
                                 racism = c(30), # 
                                 health_care = c(31),
                                 swedishness = c(32,944), # fix post-hoc
                                 language = c(33),
                                 balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
    
  }else if(current_model =='2021-06-24--09_02_42'){
    agg_topics <- list(tmp= list(immigration = c(0),
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
                                 swedishness = c(32,271),  # fix post-hoc
                                 language = c(33),
                                 balkan = c(34)))
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism',
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
    
  } else if(current_model%in%c('2024-01-11--16_01_43','90_1')){
    agg_topics <- list(tmp= list(immigration = c(0),
                     crime = c(1:4, 489),
                     education = c(5:6, 951),
                     human_rights = c(7:8,35), #7: about the royal family?
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
                     racism = c(30,809), 
                     health_care = c(31),
                     swedishness = c(32),
                     language = c(33, 937),
                     balkan = c(34)))
    
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    
    topicid <- 1
  }else if(current_model%in%c('2024-01-22--00_44_20','80_1')){
    agg_topics <- list(tmp= list(immigration = c(0),
                     crime = c(1:4, 290),
                     education = c(5:6, 343),
                     human_rights = c(7:8,35), #7:schooling for orphan children (also education?) 
                     eu = c(9),
                     families = c(10:11),
                     labour_market = c(12:14),
                     discrimination = c(15), #gender equality
                     finance = c(16:17),
                     political_parties = c(18:21),
                     terrorism = c(22:23), 
                     multiculturalism = c(24), #democracy
                     housing = c(25),
                     iraq = c(26),
                     syria = c(27),
                     israel_palestine = c(28),
                     religion = c(29,233),
                     racism = c(30,68),#WW2 
                     health_care = c(31),
                     swedishness = c(32),
                     language = c(33),
                     balkan = c(34)))
    
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
  } else if(current_model%in%c('2024-01-27--11_21_40','90_2')){
    agg_topics <- list(tmp= list(immigration = c(0),
                     crime = c(1:4),
                     education = c(5:6, 166),
                     human_rights = c(7:8,35), #7:schooling for orphan children (also education?) 
                     eu = c(9),
                     families = c(10:11),
                     labour_market = c(12:14),
                     discrimination = c(15), #gender equality
                     finance = c(16:17),
                     political_parties = c(18:21),
                     terrorism = c(22:23), 
                     multiculturalism = c(24), 
                     housing = c(25),
                     iraq = c(26),
                     syria = c(27),
                     israel_palestine = c(28),
                     religion = c(29, 254),
                     racism = c(30), #WW2 
                     health_care = c(31),
                     swedishness = c(32, 398), 
                     language = c(33),
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 1
  } else if(run%in%c('2024-01-28--10_33_18','70_2')){
    agg_topics <- list(tmp= list(immigration = c(0),
                     crime = c(1:4, 616), 
                     education = c(5:6, 108),
                     human_rights = c(7:8), 
                     eu = c(9),
                     families = c(10:11),
                     labour_market = c(12:14),
                     discrimination = c(15), #gender equality
                     finance = c(16:17),
                     political_parties = c(18:21),
                     terrorism = c(22:23), 
                     multiculturalism = c(24), 
                     housing = c(25),
                     iraq = c(26),
                     syria = c(27),
                     israel_palestine = c(28),
                     religion = c(29, 378),
                     racism = c(30), #WW2 
                     health_care = c(31),
                     swedishness = c(32, 418), 
                     language = c(33),
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 1
  }else if(run%in%c('2024-02-03--09_18_24','70_3')){
    agg_topics <- list(tmp= list(immigration = c(0),
                     crime = c(1:4, 616), 
                     education = c(5:6),
                     human_rights = c(7:8), 
                     eu = c(9),
                     families = c(10:11),
                     labour_market = c(12:14),
                     discrimination = c(15), #gender equality
                     finance = c(16:17),
                     political_parties = c(18:21),
                     terrorism = c(22:23), 
                     multiculturalism = c(24), 
                     housing = c(25),
                     iraq = c(26),
                     syria = c(27),
                     israel_palestine = c(28),
                     religion = c(29, 378),
                     racism = c(30), #WW2 
                     health_care = c(31),
                     swedishness = c(32,185,224), 
                     language = c(33),
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 1
  } else if(run%in%c('2024-02-05--12_14_55','80_3')){
    agg_topics <- list(tmp= list(immigration = c(0),
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
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
  } else if(run%in%c('2024-02-08--09_09_36','90_3')){
    agg_topics <- list(tmp= list(immigration = c(0),
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
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
  } else if(run%in%c('2024-02-08--09_09_45','70_1')){
    agg_topics <- list(tmp= list(immigration = c(0),
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
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
  }else if(run%in%c('2024-01-28--10_33_25','80_2')){
    agg_topics <- list(tmp= list(immigration = c(0),
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
                     balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
  }
  
  
  
  # collect all non-seeded columns as "other"
  nc <- (ncol(data[,names(data)%like%'T_', with = F])-1)
  agg_topics[[1]]$other <- c(0:nc)[-which(c(0:nc)%in%unlist(agg_topics[[1]]))]
  # extract list names to all seeded topics (+other)
  names(agg_topics) <- current_model
  topics <- agg_topics[[1]]
  
  
  # get number of seeded topics per docs.
  combined_data <- c()
  #data <- data.table(data)
  for(i in 1:length(topics)){
    tmp_topics <- topics[i][[1]]
    if(!any(is.na(tmp_topics))){
      combined_data <- cbind(combined_data,rowSums(data[,paste0('T_', tmp_topics), with = F]))
    } else {
      combined_data <- cbind(combined_data, rep(0, nrow(data)))
    }
  }
  
  # add id-variables
  combined_data <- data.frame(combined_data)
  names(combined_data) <- names(topics)
  combined_data$id <- data$id
  combined_data$date <- data$date
  combined_data$paper <- data$paper
 
  # aggregate data per date & paper 
  data2 <- data.table(combined_data)
  data2 <- data2[order(data2$date),]
  df_profile <- data2 %>% dplyr::select(-id) %>% data.table
  df_profile <- df_profile[, lapply(.SD, sum, na.rm = T), by = c('date','paper')]
  
  # convert to long format
  df_profile <- melt(df_profile, id.vars = c('date','paper'), variable.name = 'profile',value.name = 'n')
  
  # combine different seeded topics into 5 main frames
  df_profile[, profile := as.character(profile)]
  df_profile[,large_profile0:=ifelse(profile%in%c('education','finance','housing','health_care','labour_market'),'economy',
                                     ifelse(profile%in%c('human_rights','families','discrimination','neo_nazism','racism'),'humanitarian',
                                            ifelse(profile%in%c('terrorism','crime'),'security',
                                                   ifelse(profile%in%c('multicultarlism','islam', 'religion','cultural_aspects','swedishness','multiculturalism','language'),'cultural',
                                                          ifelse(profile%in%c('eu','political_parties'),'politics','other')))))]

  # remove all that are not seeded
  df_profile <- df_profile[large_profile0!='other',]
  # sum per date/journal
  df_N <- df_profile[,sum(n), by = c('date','paper')]
  names(df_N) <- c('date','paper','N')
  # sum per date/journal/frame
  df_n <- df_profile[,sum(n), by = c('date','paper','large_profile0')]
  names(df_n) <- c('date','paper','large_profile0','large_profile_n')
  # combine and calculate proportions
  df_profile <- merge(df_n,df_N, by = c('date','paper'))
  df_profile[, r := large_profile_n/N]
  # convert to date varaianle
  df_profile[, date := as.Date(date)]
  # replace Nan (due to row sum 0) to 0
  df_profile[,r := ifelse(is.nan(r),0,r)]
  # add year variable
  df_profile[, year := lubridate::year(date)]
  
  # change names
  setnames(df_profile, "large_profile0", "frame")
  setnames(df_profile, "large_profile_n", "frame_n")
  setnames(df_profile, "r", "frame_r")
  
  # re-shuffle frame order (to make plot prettier)
  df_profile$frame<- factor(df_profile$frame, 
                                           levels = c("humanitarian", 
                                                      "security", 
                                                      "economy",
                                                      'cultural',
                                                      'politics'))
  gc()
  return(df_profile)
}



#----------------------------------------------------------------------------#
#                 Format & extract event raw data                 ----
#----------------------------------------------------------------------------#

get_event_raw_data <- function(run ="2021-06-06--18_11_45", data = data) {
  
  # which colums are seeded with which frames (depend on model run, due to some post-hoc fixes -- see supplementary material)
  if(run =='2021-06-06--18_11_45'){
    agg_topics <-  list(tmp = list(immigration = c(0),
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
                                   racism = c(30,707), # fix post-hoc
                                   health_care = c(31),
                                   swedishness = c(785), # fix post-hoc
                                   language = c(33),
                                   balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
    
  }else if(run =='2021-06-18--19_18_47'){
    agg_topics <-  list(tmp=list(immigration = c(0),
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
                                 racism = c(30), # 
                                 health_care = c(31),
                                 swedishness = c(32,944), # fix post-hoc
                                 language = c(33),
                                 balkan = c(34)))
    
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism', 
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
    
  }else if(run =='2021-06-24--09_02_42'){
    agg_topics <- list(tmp= list(immigration = c(0),
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
                                 swedishness = c(32,271),  # fix post-hoc
                                 language = c(33),
                                 balkan = c(34)))
    topicnames <- c('immigration', 'crime',  'education', 'human_rights',
                    'eu', 'families','labour_market','discrimination',
                    'finance','political_parties', 'terrorism', 'multiculturalism',
                    'housing', 'iraq', 'syria', 'israel_palestine','religion',
                    'racism', 'health_care', 'swedishness', 'language','balkan')
    topicid <- 0
    
  }
  
  # collect all non-seeded columns as "other"
  nc <- (ncol(data[,names(data)%like%'T_', with = F])-1)
  agg_topics[[1]]$other <- c(0:nc)[-which(c(0:nc)%in%unlist(agg_topics[[1]]))]
  # extract list names to all seeded topics (+other)
  names(agg_topics) <- runs[run_nr]
  topics <- agg_topics[[1]]
  
  
  # get number of seeded topics per docs.
  combined_data <- c()
  #data <- data.table(data)
  for(i in 1:length(topics)){
    tmp_topics <- topics[i][[1]]
    if(!any(is.na(tmp_topics))){
      combined_data <- cbind(combined_data,rowSums(data[,paste0('T_', tmp_topics), with = F]))
    } else {
      combined_data <- cbind(combined_data, rep(0, nrow(data)))
    }
  }
  
  # add id-variables
  combined_data <- data.frame(combined_data)
  names(combined_data) <- names(topics)
  combined_data$id <- data$id
  combined_data$date <- data$date
  combined_data$paper <- data$paper
  
  gc()
  return(combined_data)
}



#----------------------------------------------------------------------------#
#             Aggregate daily framing salience to yearly                ----
#----------------------------------------------------------------------------#

get_yearly_framing_salience <- function(df_frame_salience_plot = df_frame_salience) {

  # remove colums we don't need here
  df_frame_salience_plot[, N := NULL]
  df_frame_salience_plot[, date := NULL] 
  
  # remove "other" frae
  df_frame_salience_plot <- df_frame_salience_plot[frame!='other',]
  # sum per year
  df_N <- df_frame_salience_plot[,sum(frame_n), by = c('year')]
  names(df_N) <- c('year','N')
  # sum per frame/year
  df_n <- df_frame_salience_plot[,sum(frame_n), by = c('frame','year')]
  names(df_n) <- c('frame','year','frame_n')
  # merge & calculate proportion
  df_frame_salience_plot <- merge(df_n,df_N, by = c('year'))
  df_frame_salience_plot[, frame_r := frame_n/N]
  # order by year
  df_frame_salience_plot <- df_frame_salience_plot[order(year),]
  
  return(df_frame_salience_plot)
}


#----------------------------------------------------------------------------#
#             Plot Fig.2a time series of framing salience                ----
#----------------------------------------------------------------------------#


plot_framing_salience <-  function(df_plot = df_frame_salience_plot){
  df_plot %>% 
    arrange(year) %>%
    group_by(frame) %>%
    mutate(newcol = rollapply(frame_r, 5, mean, fill = NA)) %>%
    data.table()-> test
  test[,newcol:= ifelse(is.na(newcol),frame_r,newcol)]
  
  # create colorblind friendly colors
  cbPalette <- c(   )
  
  
  test%>%
    filter(frame!='other') %>%
    ggplot(aes(x = year, y = frame_r, col = frame)) +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
    geom_line(aes(x = year, y = newcol,col = frame)) +
    theme(aspect.ratio=1) +
    scale_color_manual(values=c('cultural' = "#440154FF", 
                                'economy' = "#3B528BFF",
                                'humanitarian' = "#21908CFF",
                                'security' = "#5DC863FF",
                                'politics' = "#FDE725FF"),
                       labels = c('cultural' = "Culture", 
                                  'economy' = "Economy", 
                                  'humanitarian' ="Human \nrights",
                                  'security' = "Security",
                                  'politics' ="Politics")) +
    scale_fill_manual(values=c('cultural' = "#440154FF", 
                               'economy' = "#3B528BFF",
                               'humanitarian' = "#21908CFF",
                               'security' =   "#5DC863FF",
                               'politics' = "#FDE725FF"),
                      labels = c('cultural' = "Culture", 
                                 'economy' = "Economy", 
                                 'humanitarian' ="Human \nrights",
                                 'security' = "Security",
                                 'politics' ="Politics")) +
    theme_bw(base_size = 11) +
    theme(legend.position="right",
          legend.title = element_blank(),
          axis.text.x=element_blank()) +
    geom_point(size = 1) +
    scale_y_continuous(breaks = c(0.1, 0.2,0.3,0.4,0.5,0.6),
                       labels =  c('0.1', '0.2','0.3','0.4','0.5',0.6))+ 
    labs(x = '',
         y = '')  +
    scale_x_continuous(limits = c(1945, 2020),
                       expand = c(0,0))  -> p0a2
  
  p0a2 + theme(plot.margin = unit(c(0.3,0.3,-0.3,0.3), "lines"), 
        legend.position = 'bottom',
        axis.title.y = element_text(size = 11)) +
    labs(y = 'Interpretive frame') -> p0a3
  
  return(p0a3)
}

#----------------------------------------------------------------------------#
#                             Extract legend                             ----
#----------------------------------------------------------------------------#
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }

#----------------------------------------------------------------------------#
#              Run multivariate turning point analysis                   ----
#----------------------------------------------------------------------------#

get_multivariate_tp <- function(tmp_data = df_frame_salience_yearly_wide){
  
  # create matrix & removing year col.
  mat_frame_salience_yearly_wide <- as.matrix(tmp_data[,-which(names(tmp_data)=='year')])
  
  # run analysis
  tmp_multivariate <-  bcp(cbind(mat_frame_salience_yearly_wide),  burnin = 75000,  mcmc = 700000)
  
  # save in data frame
  multivariate_bcp <- data.table(posterior_prob = round(tmp_multivariate$posterior.prob, digits = 3),
                                 year = tmp_data$year)
  return(multivariate_bcp)
}

#----------------------------------------------------------------------------#
#              Run univariate turning point analysis                   ----
#----------------------------------------------------------------------------#

get_univariate_tp <- function(tmp_data = df_frame_salience_yearly_wide, frame_name= 'security'){
  
  # create matrix & removing year col.
  mat_frame_salience_yearly_wide <- as.matrix(tmp_data[,-which(names(tmp_data)=='year')])
  
  # run analysis
  tmp_uni  <- bcp(cbind(mat_frame_salience_yearly_wide[,colnames(mat_frame_salience_yearly_wide)==frame_name]), burnin = 20000,  mcmc = 200000, return.mcmc = TRUE)
 
   #save in data frame
  tmp_uni <- data.frame(posterior_prob = tmp_uni$posterior.prob, posterior_mean = tmp_uni$posterior.mean, year = tmp_data$year, frame = frame_name)
  gc()
  return(tmp_uni)
}


#----------------------------------------------------------------------------#
#                        Plot Fig.2b turning points                    ----
#----------------------------------------------------------------------------#

plot_turning_points <-  function(df_plot = univariate_bcp, turning_points =  tp_year, run = "2021-06-06--18_11_45"){

  ggplot(df_plot) +
  geom_line(aes(x = year, y = posterior_prob, col = frame), size = 1, alpha = 0.5) +
  theme(aspect.ratio=1,
        legend.position = 'none') +
  theme_bw(base_size = 11) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2),
                     expand=expand_scale(mult=c(0,0.1))) + 
  scale_x_continuous(limits = c(1945, 2020),
                     breaks = c(turning_points[order(turning_points)]),
                     expand = c(0,0)) + 
  labs(y = '',
       x = '',
       color = '')  +
  scale_color_manual(values=c('cultural' = "#440154FF", 
                              'economy' = "#3B528BFF" ,
                              'humanitarian' = "#21908CFF",
                              'security' = "#5DC863FF",
                              'politics' = "#FDE725FF"),
                     labels = c('cultural' = "Culture", 
                                'economy' = "Economy", 
                                'humanitarian' ="Human \nrights",
                                'security' = "Security",
                                'politics' ="Politics")) -> tmp_plt
  
  if(run=="2021-06-06--18_11_45"){
    tmp_plt +
    annotate('rect', xmin = 1945, xmax = 1955, ymin = 0, ymax = Inf,
             alpha = 0.1,
             fill= "skyblue") +
      annotate('rect', xmin = 1955, xmax = 1965, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      
      annotate('rect', xmin = 1965, xmax = 1974, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      
      annotate('rect', xmin = 1974, xmax = 1986, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      annotate('rect', xmin = 1986, xmax = 2000, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      annotate('rect', xmin = 2000, xmax = 2013, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      annotate('rect', xmin = 2013, xmax = 2019, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      geom_line(data = multivariate_bcp,aes(x = year, y = posterior_prob), size = 0.5)  +
      geom_hline(yintercept = 0.5, lty = 'dotted')  -> tp_both
  }else if(run=="2021-06-18--19_18_47"){
    tmp_plt +
      annotate('rect', xmin = 1945, xmax = 1948, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      annotate('rect', xmin = 1948, xmax = 1956, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      
      annotate('rect', xmin = 1956, xmax = 1965, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      
      annotate('rect', xmin = 1965, xmax = 1984, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      annotate('rect', xmin = 1984, xmax = 2008, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      annotate('rect', xmin = 2008, xmax = 2013, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      annotate('rect', xmin = 2013, xmax = 2019, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      geom_line(data = multivariate_bcp,aes(x = year, y = posterior_prob), size = 0.5)  +
      geom_hline(yintercept = 0.5, lty = 'dotted')  -> tp_both
  }else if(run=="2021-06-24--09_02_42"){
    tmp_plt +
      annotate('rect', xmin = 1945, xmax = 1948, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      annotate('rect', xmin = 1948, xmax = 1955, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      
      annotate('rect', xmin = 1955, xmax = 1965, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      
      annotate('rect', xmin = 1965, xmax = 1986, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      annotate('rect', xmin = 1986, xmax = 2000, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      annotate('rect', xmin = 2000, xmax = 2009, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      annotate('rect', xmin = 2009, xmax = 2013, ymin = 0, ymax = Inf,
               alpha = 0.1,
               fill= "skyblue") +
      annotate('rect', xmin = 2013, xmax = 2019, ymin = 0, ymax = Inf,
               alpha = 0.3,
               fill= "grey") +
      geom_line(data = multivariate_bcp,aes(x = year, y = posterior_prob), size = 0.5)  +
      geom_hline(yintercept = 0.5, lty = 'dotted')  -> tp_both
  }

  tp_both + theme(plot.margin = unit(c(-0.3,0.3,0.3,0.3), "lines"), 
          legend.position = 'none') +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1),
    theme(axis.title.y = element_text(size = 11))+
    labs(y = 'Posterior prob.') -> tp_both
  
  return(tp_both)
}



#----------------------------------------------------------------------------#
#                   Extract weekly framing salience data                  ----
#----------------------------------------------------------------------------#

get_weekly_framing_salience <- function(tmp_data = combined_data){
  # create date variables
  data2 <- data.table(tmp_data)
  data2 <- data2[order(data2$date),]
  data2[,year:=lubridate::year(date)]
  data2[,week:=lubridate::week(date)]

  # sum all obs per year-week
  df_profile <- data2 %>% dplyr::select(-id,-date) %>% data.table
  df_profile <- df_profile[, lapply(.SD, sum, na.rm = T), by = c('year','week','paper')]
  
  # create long format
  df_profile <- melt(df_profile, id.vars = c('year','week','paper'), variable.name = 'profile',value.name = 'n')

  # agrgegate seeded topics to 5 frames
  df_profile[, profile := as.character(profile)]
  df_profile[,frame:=ifelse(profile%in%c('education','finance','housing','health_care','labour_market'),'economy',
                                     ifelse(profile%in%c('human_rights','families','discrimination','neo_nazism','racism'),'humanitarian',
                                            ifelse(profile%in%c('terrorism','crime'),'security',
                                                   ifelse(profile%in%c('multicultarlism','islam', 'religion','cultural_aspects','swedishness','multiculturalism','religion_islam','language'),'cultural',
                                                          ifelse(profile%in%c('eu','political_parties'),'politics','other')))))]
  
  
  
  
  # remove all that are not seeded
  df_profile <- df_profile[frame!='other',]
  # sum per week/journal
  df_N <- df_profile[,sum(n), by = c('year','week','paper')]
  names(df_N) <- c('year','week','paper','N')
  # sum per week/journal/frame
  df_n <- df_profile[,sum(n), by = c('year','week','paper','frame')]
  names(df_n) <- c('year','week','paper','frame','frame_n')
  # merge
  df_profile <- merge(df_n,df_N, by = c('year','week','paper'))
  # create frame proportions
  df_profile[, frame_r := frame_n/N]
  df_profile[, frame_r := ifelse(is.nan(frame_r),0,frame_r)]
  
  return(df_profile)
  
}



#----------------------------------------------------------------------------#
#             Event case study: Bayesian fixed effect model              ----
#----------------------------------------------------------------------------#

bayes_fixed_effect_avg <- function(frame_type = 'humanitarian',
                                     no_compare_frame = 'other',
                                     event_data = events_df_tmp,
                                     profile_data = df_profile,
                                     pre_time_window = 20,
                                     post_time_window = 20,
                                     pre_nr_weeks = 1,
                                     post_nr_weeks = 1,
                                     n_chains = 8,
                                     n_iter = 4000){
    
    pdata <- pdata2 <-plot_data <-  NULL
    for(e in 1:nrow(event_data)){
      year_periods <- unique(df_profile[,c('year','week')])
      event <- as.Date(event_data$start_date[e])
      event_year <- lubridate::year(event)
      event_week <- lubridate::week(event)
      event_name <- event_data$event_names[e]
      print(event_name)
      
      # data selection (period before and after intervention)
      first_date <- max(ymd(event-pre_time_window),ymd(min('1945-01-01')))
      last_date <- min(ymd(event+post_time_window),ymd('2019-05-31'))
      window <- seq(first_date,last_date,by = 1)
      window_year <- unique(lubridate::year(window))
      window_week <- unique(lubridate::week(window))
      
      
      if(length(window_year)>1){
        year_week <- data.frame(year = rep(window_year[1],length(window_week[window_week>45])),
                                week = window_week[window_week>45])
        year_week <- rbind(year_week,
                           data.frame(year = rep(window_year[2],length(window_week[window_week<=45])),
                                      week = window_week[window_week<=45]))
        
      }else{
        year_week <- data.frame(year = rep(window_year,length(window_week)),
                                week = window_week)
        
      }
      
      tmp_data <- NULL
      for(i in 1:nrow(year_week)){
        tmp_data <- rbind(tmp_data,
                          profile_data[year==year_week$year[i] & week == year_week$week[i],])
      }
      
      
      tmp_data <- unique(tmp_data)
      
      # create time dummies
      tmp_data <- data.frame(tmp_data)
      for(ttt in pre_nr_weeks:1){
        tmp_data[,eval(paste0('t0_wm',ttt))] <- ifelse(tmp_data$week == event_week-ttt,1,0)
      }
      
      for(ttt in post_nr_weeks:1){
        tmp_data[,eval(paste0('t0_wp',ttt))] <- ifelse(tmp_data$week == event_week+ttt,1,0)
      }
      
      
      tmp_data[,'t0'] <- ifelse(tmp_data$week==event_week,1,0)
      
      #copy data for two effects
      tmp_data2 <- tmp_data
      
      # put time dummies to 0 for untreated
      tmp_data[tmp_data$frame!=frame_type,names(tmp_data)%like%'t0'] <- 0
      tmp_data$log_r <- log(tmp_data$frame_r+0.0000000001)
      
      
      # put time dummies to 0 for untreated - placebo
      tmp_data2[tmp_data2$frame==no_compare_frame,names(tmp_data)%like%'t0'] <- 0
      tmp_data2$log_r <- log(tmp_data2$frame_r+0.0000000001)
      
      
      
      #create id
      tmp_data$id <- paste0(tmp_data$frame,'_',tmp_data$paper,'_',e) #
      tmp_data2$id <- paste0(tmp_data$frame,'_',tmp_data$paper,'_',e) # , e)
      tmp_data$event <- event_name
      pdata <- rbind(pdata,tmp_data)
      pdata2 <- rbind(pdata2,tmp_data2)
      
    }
    
    
    pdata <- unique(pdata)
    message(paste0('N:',print(nrow(pdata))))
    pdata2 <- unique(pdata2)
    pdata$event <- pdata2$event <- NULL
    
    # create panel data
    pdata$time <- paste0(pdata$year,'-',pdata$week)
    pdata2$time <- paste0(pdata2$year,'-',pdata2$week)
    pdata$ind <- as.factor(paste0(pdata$id,'_',pdata$time))
    

    mod <- stan_glm(formula=
                      log_r ~   1 + 
                      id +
                      t0_wm1 + 
                      t0 + 
                      t0_wp1 + 
                      t0_wp2 ,
                    data = pdata, 
                    iter = n_iter, 
                    chains = n_chains, 
                    #warmup = 2000,
                    cores = 4,
                    prior = normal(0,100),
                    prior_intercept = normal(0,100))
    
    print(summary(mod, probs = c(0.05, 0.95)))
    post_intervals <- mcmc_intervals(mod, 
                                     point_est ='median',
                                     pars = c("t0_wm1", "t0", "t0_wp1", "t0_wp2"),
                                     prob = 0.5,
                                     prob_outer = c(0.90)) # use 90% credible bands
    post_intervals 
    draws <- as.matrix(mod)
    
    
    if(no_compare_frame!=''){
      message(paste0('N2:',print(nrow(pdata2))))
      mod2 <- stan_glm(formula=
                         log_r ~ 
                         1 + 
                         id +
                         t0_wm1  +
                         t0  +
                         t0_wp1  +
                         t0_wp2  ,
                       data = pdata2, 
                       iter = n_iter, 
                       chains = n_chains, 
                       cores = 4,
                       #warmup = 2000,
                       prior = normal(0,100),
                       prior_intercept = normal(0,100))
      
      print(summary(mod2, probs = c(0.05, 0.95)))
      post_intervals2 <- mcmc_intervals(mod2, 
                                        pars = c("t0_wm1", "t0", "t0_wp1", "t0_wp2"),
                                        prob = 0.5,
                                        prob_outer = 0.90)
      draws2 <- as.matrix(mod2)
    }
    
    
    if(no_compare_frame!=''){
      res <- list(su_type = post_intervals,
                  no_compare_su = post_intervals2,
                  draws_su_type = draws,
                  draws_compare = draws2) 
    }else{
      res <- list(su_type = post_intervals,
                  draws_su_type = draws) 
    }
    
    return(res)
    
}
  


#----------------------------------------------------------------------------#
#             Plot event study             ----
#----------------------------------------------------------------------------#

plot_event_study <- function(event0 = swe_terror[[1]]$data, event1 = int_terror[[1]]$data, type0 = 'swe', type1 = 'int', col0 = '#c7bd36', col1 = '#F0E442', event_name = 'Islamic terror'){
  
  # extract output from events
  params <- c(-1,0,1,2)
  event0$type2 <- type0
  event0$parameter <-  params 
  event1$type2 <- type1
  event1$parameter <- params 
  
  # combine from the 2 events
  tmp_dat<- rbind(event0, event1)
  
 # plot
  tmp_dat%>%
    ggplot(aes(x = parameter, y = m, group = type2))+
    geom_errorbar(aes(ymin = ll, ymax = hh), 
                  size = 0.2, width=0, 
                  position = position_dodge(width = 0.4)) +
    geom_hline(yintercept = 0, linetype = 1) +
    theme_bw(base_size = 20) + 
    scale_color_manual(values = c(col0,col1,'white'),
                       limits = c(type0, type1,'white'))  +
    scale_fill_manual(values = c(col0,col1),
                      limits = c(type0, type1)) + 
    labs(color = '') +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, col = 'red', linetype =2)+
    geom_line(aes( col= type2), size = 0.9,position = position_dodge(width = 0.4)) +
    geom_point(aes(fill = type2, col= 'white'),
               size = 1.5,shape = 21,position = position_dodge(width = 0.4), 
               stroke = 1)+
    theme(legend.position="none",
          title =element_text(size=10, face='plain')) +
    labs(y = '',
         x = '',
         title = event_name) -> event_plot
  return(event_plot)
}

#----------------------------------------------------------------------------#
#             Plot event study             ----
#----------------------------------------------------------------------------#


exponentiate_event_effects <- function(event0 = swe_terror[[1]]$data, event1 = int_terror[[1]]$data, type0 = 'swe', type1 = 'int'){
  
  # extract output from events
  params <- c(-1,0,1,2)
  event0$type2 <- type0
  event0$parameter <-  params 
  event1$type2 <- type1
  event1$parameter <- params 
  
  # combine from the 2 events
  tmp_dat <- rbind(event0, event1)
  
  # exponential
  tmp_dat$m_exp <- exp(tmp_dat$m)
  tmp_dat$ll_exp <- exp(tmp_dat$ll)
  tmp_dat$hh_exp <- exp(tmp_dat$hh)
  return(tmp_dat)
}




plot_event_study_exp <- function(event0 = swe_terror[[1]]$data, event1 = int_terror[[1]]$data, type0 = 'swe', type1 = 'int', col0 = '#c7bd36', col1 = '#F0E442', event_name = 'Islamic terror'){
  
  # get data
  tmp_dat <- exponentiate_event_effects(event0 = event0, event1 = event1, type0 = type0, type1 = type1)

  # plot
  tmp_dat%>%
    ggplot(aes(x = parameter, y = m_exp, group = type2))+
    geom_errorbar(aes(ymin = ll_exp, ymax = hh_exp), 
                  size = 0.2, width=0, 
                  position = position_dodge(width = 0.4)) +
    geom_hline(yintercept = 1, linetype = 1) +
    theme_bw(base_size = 20) + 
    scale_color_manual(values = c(col0,col1,'white'),
                       limits = c(type0, type1,'white'))  +
    scale_fill_manual(values = c(col0,col1),
                      limits = c(type0, type1)) + 
    labs(color = '') +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, col = 'red', linetype =2)+
    geom_line(aes( col= type2), size = 0.9,position = position_dodge(width = 0.4)) +
    geom_point(aes(fill = type2, col= 'white'),
               size = 1.5,shape = 21,position = position_dodge(width = 0.4), 
               stroke = 1)+
    theme(legend.position="none",
          title =element_text(size=10, face='plain')) +
    labs(y = '',
         x = '',
         title = event_name) -> event_plot
    
  
  return(event_plot)
}

#----------------------------------------------------------------------------#
#    ADDED:8/2-24         Plot event study - SINGLE EVENTS            ----
#----------------------------------------------------------------------------#


exponentiate_event_effects_single<- function(event0 = swe_terror[[1]]$data, type0 = 'swe'){
  
  # extract output from events
  params <- c(-1,0,1,2)
  event0$type2 <- type0
  event0$parameter <-  params 
  
  # combine from the 2 events
  tmp_dat <- event0
  
  # exponential
  tmp_dat$m_exp <- exp(tmp_dat$m)
  tmp_dat$ll_exp <- exp(tmp_dat$ll)
  tmp_dat$hh_exp <- exp(tmp_dat$hh)
  return(tmp_dat)
}





plot_event_study_exp_single <- function(event0 = swe_terror[[1]]$data,  type0 = 'swe',  col0 = '#c7bd36',  event_name = 'Islamic terror'){
  
  # get data
  tmp_dat <- exponentiate_event_effects_single(event0 = event0, type0 = type0)
  
  # plot
  tmp_dat%>%
    ggplot(aes(x = parameter, y = m_exp, group = type2))+
    geom_errorbar(aes(ymin = ll_exp, ymax = hh_exp), 
                  size = 0.2, width=0, 
                  position = position_dodge(width = 0.4)) +
    geom_hline(yintercept = 1, linetype = 1) +
    theme_bw(base_size = 20) + 
    scale_color_manual(values = c(col0,'white'),
                       limits = c(type0, 'white'))  +
    scale_fill_manual(values = c(col0),
                      limits = c(type0)) + 
    labs(color = '') +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, col = 'red', linetype =2)+
    geom_line(aes( col= type2), size = 0.9,position = position_dodge(width = 0.4)) +
    geom_point(aes(fill = type2, col= 'white'),
               size = 1.5,shape = 21,position = position_dodge(width = 0.4), 
               stroke = 1)+
    theme(legend.position="none",
          title =element_text(size=10, face='plain')) +
    labs(y = '',
         x = '',
         title = event_name) -> event_plot
  
  
  return(event_plot)
}




