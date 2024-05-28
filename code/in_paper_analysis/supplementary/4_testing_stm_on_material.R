# try out STM for our data on login node NAISS (32 cores, ~400GB RAM)
# DUE TO DATA STORAGE LIMITATIONS ON GITHUB WE DO NOT PROVIDE THE DATA
# FOR THIS ROBUSTNESS ANALYSIS
data_path <- '/data/miriam/eras-data/full_data/'
data_file <- 'part1_small_dtm.txt'

library('data.table')

data <- fread(paste0(data_path,data_file))
names(data) <- c('id','date','content')
# create journal variable
data$paper <- ifelse(substr(data$id,start = 1, stop = 1)=='A','Aftonbladet',
                     ifelse(substr(data$id,start = 1, stop = 1)=='E', 'Expressen',
                            ifelse(substr(data$id,start = 1, stop = 1)=='S','Svenska Dagbladet','Dagens Nyheter')))
# dropping ID & date variables (do not use)
data[,id:= NULL]
data[,date:= NULL]
print(data[1,])
gc()

library('stm')
# following the Vignette for STM package (https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf)

# reading data
start_time <- Sys.time()
processed <- textProcessor(data$content, 
                           metadata = NULL,
                           lowercase = FALSE,
                           removestopwords = FALSE,
                           removenumbers = FALSE,
                           removepunctuation = FALSE,
                           ucp = FALSE,
                           stem = FALSE,
                           wordLengths = c(1, Inf),
                           language = 'swe')
out <- prepDocuments(processed$documents,processed$vocab)
out 
docs <- out$documents 
vocab <- out$vocab 
meta <- out$meta
end_time <- Sys.time()
time_taken <- round(end_time - start_time,2)
print(time_taken)
message('preproccessing done')
gc()

# run model with K=1000
start_time <- Sys.time()
message('run model')
modelFit <- stm(documents = out$documents,
               vocab = out$vocab, 
               K = 1000,
               max.em.its= 75,
               data = out$meta)
end_time <- Sys.time()
time_taken <- round(end_time - start_time,2)
time_taken
print(time_taken)
