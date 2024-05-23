library(data.table)
library(tidyr)
library(parallel)

#-----------
# SET UP
#-----------
args <- commandArgs(trailingOnly = TRUE)
proj_name <- args[1]
run_nr <- args[2]
chunk_path <-paste0('/proj/',proj_name,'/model_output/z_files/Run',run_nr,'/')
print(chunk_path)

# read data
message('Read in data and chunk it')
if(proj_name=='efe_et'){
  # added 1/2-24 (after update NSC --> NAISS)
  data <- fread(paste0('/proj/',proj_name,'/old_m4m/data/part1_small_dtm.txt'), header = F, sep = '\t')
} else {
  data <- fread(paste0('/proj/',proj_name,'/data/part1_small_dtm.txt'), header = F, sep = '\t')
}
names(data) <- c('id','date','content')

# split data
n <- 10000
nr <- nrow(data)
system.time(dt <- split(data, rep(1:ceiling(nr/n), each=n, length.out=nr)))
names(dt) <- 1:length(dt)

add_chunk_ind <- function(ind, data){
  data$chunk_ind <- (ind-1)
  return(data)
}

dt <- lapply(1:length(dt), function(i) add_chunk_ind(ind = i, data = dt[[i]]))


#function
remove_na_and_match <- function(chunk_path = chunk_path,
                                tmp_data = dt[[1]]){
  
  chunk_nr <- unique(tmp_data$chunk_ind)
  tmp_data[, chunk_ind := NULL]
  tmp <- fread(paste0(chunk_path,'chunk',chunk_nr,'.csv'))
  
  if(ncol(tmp)>4){
    tmp <- tmp[,1:2]
    z_length <- nrow(tmp)
    tmp <- separate(tmp, col = 'w_0', sep = ',', into = paste0('w_',0:14999))
    
    tmp_id <- unique(tmp_data$id)
    #tmp <- tmp[1:1000,]
    tmp$id <- tmp_id
    
    tmp_data <- tmp_data[,strsplit(content, ' ', perl = T), by = c('id','date')]
    names(tmp_data) <- c('id','date','token')
    tmp_data[, token_id := 1:.N, by = 'id']
    tmp_data[,token_id := token_id - 1]
    
    
    tmp <- tmp %>% pivot_longer(cols = starts_with("w_"), names_to = "col_id", values_to = "z", values_drop_na = T) %>% data.table()
    #tmp <- tmp[!is.na(z),]
    names(tmp) <- c('row_id','id', 'token_id','z')
    
    setorder(tmp, row_id)
    tmp[, token_id := as.numeric(gsub(pattern = 'w_', replacement = '', x = token_id))]
    setorder(tmp, row_id,token_id)
    
    tmp <- merge(tmp_data, tmp, by = c('id','token_id'))
    setorder(tmp, row_id, token_id)
    
    tmp[, token_id := NULL]
    tmp[, row_id := NULL]
    
    if(nrow(tmp_data)!=nrow(tmp)){message(paste0('Error in chunk_nr (tmp_data): ', chunk_nr))}
    if(z_length!=nrow(tmp)){message(paste0('Error in chunk_nr (tmp_z): ', chunk_nr))}
    fwrite(tmp, paste0(chunk_path,'chunk',chunk_nr,'.csv'))
    
  }
  gc()
  return('')
}

message('Parallel matching')
cores <- detectCores() - 1
cl <- makeCluster(cores)
var_func_list <- c('remove_na_and_match','chunk_path') 
clusterEvalQ(cl, {library(dplyr);library(tidyr); library(data.table)})
clusterExport(cl = cl, varlist = var_func_list, envir = environment())
gc()

system.time(dt <- parLapply(cl = cl,
                            X = dt,
                            fun = function(x) try(remove_na_and_match(chunk_path = chunk_path,
                                                                      tmp_data = x))))

stopCluster(cl)
gc()
