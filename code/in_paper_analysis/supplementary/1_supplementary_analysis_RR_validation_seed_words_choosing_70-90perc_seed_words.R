
#----------------------------------------------------------------#
# Sensitivity analysis; select 95-75% of the seed words
# used to identify the immigration topic and re-run the anlaysis.
# Repeat 3 times. 

# select 3 randomly selected percentages of the origianl keyword list
#randper <- round(runif(n = 3, min = 0, max = 1), digits = 2 )
#randper 

# read in original list of migration seed words to create new seed word lists
where <- 'kb_new'
if(where=='kb_new'){
  priors <- paste0(base_path,'bash/priors/priors_main.txt')
}


library(data.table)
priors <- fread(priors, header = FALSE, fill = TRUE)
prior_words <- as.vector(as.matrix(priors[1,-1]))
prior_words <- gsub(prior_words, pattern=',', replacement = '')
tmp <- prior_words[prior_words!='']

# randomly draw X percent of seed words * 3
tmp_seeds <- c(1234,9876, 1101)
draw_x_seedwords <- function(run_seed = tmp_seeds, tmp_share = 0.95, all_seed_words = tmp){
  x_perc_seedwords <- list()
  for(i in 1:length(run_seed)){
    set.seed(run_seed[i])
    x_perc_seedwords[[i]] <- sample(all_seed_words, size = round(length(all_seed_words)*tmp_share), replace = FALSE)
  }
  return(x_perc_seedwords)
}

p90_seeds <- draw_x_seedwords(run_seed = tmp_seeds, tmp_share = 0.9, all_seed_words = tmp)
p80_seeds <- draw_x_seedwords(run_seed = tmp_seeds, tmp_share = 0.8, all_seed_words = tmp)
p70_seeds <- draw_x_seedwords(run_seed = tmp_seeds, tmp_share = 0.7, all_seed_words = tmp)

#############################################################################################

# Use ChatGPT to turn lists into text files

##############################################################################################
# Compare the seed words selected in different draws for validation study of seed words
library(data.table)

# read in original seed words
seed_word_folder <- paste0(base_path,'bash/priors/seed_word_validation_priors/')
original_seed_word_file <- list.files(seed_word_folder)[!list.files(seed_word_folder)%like%c('perc')]
original_seed_word_file <- original_seed_word_file[original_seed_word_file%like%c('.txt')]
original_seed_words <- fread(paste0(seed_word_folder, original_seed_word_file), header = FALSE, fill = TRUE)[1,-1]
original_seed_words <- as.character(original_seed_words)
original_seed_words <- original_seed_words[!original_seed_words=='']

# read in the different sampled seed words
samples_seed_words_files <- list.files(seed_word_folder)[list.files(seed_word_folder)%like%c('perc')]
samples_seed_words <- lapply(paste0(seed_word_folder, samples_seed_words_files), function(f) fread(f, header = FALSE, fill = TRUE)[1,-1])

# divide into 70, 80, or 90 percent of seed words & clea
perc70s <- samples_seed_words[1:3]
perc70s <- lapply(perc70s, function(dfs) as.character(dfs))
perc70s <- lapply(perc70s, function(chrs) chrs[!chrs==''])

perc80s <- samples_seed_words[4:6]
perc80s <- lapply(perc80s, function(dfs) as.character(dfs))
perc80s <- lapply(perc80s, function(chrs) chrs[!chrs==''])

perc90s <- samples_seed_words[7:9]
perc90s <- lapply(perc90s, function(dfs) as.character(dfs))
perc90s <- lapply(perc90s, function(chrs) chrs[!chrs==''])



# comparing 90 % of original seed words ----------------
perc90s_1 <- perc90s[[1]] ; perc90s_2 <- perc90s[[2]]; perc90s_3 <- perc90s[[3]]
removed_seeds_perc90_1 <- original_seed_words[!original_seed_words%in%perc90s_1]
removed_seeds_perc90_2 <- original_seed_words[!original_seed_words%in%perc90s_2]
removed_seeds_perc90_3 <- original_seed_words[!original_seed_words%in%perc90s_3]
removed_seeds_perc90_1;removed_seeds_perc90_2;removed_seeds_perc90_3

# comparing 80 % of original seed words ----------------
perc80s_1 <- perc80s[[1]] ; perc80s_2 <- perc80s[[2]]; perc80s_3 <- perc80s[[3]]
removed_seeds_perc80_1 <- original_seed_words[!original_seed_words%in%perc80s_1]
removed_seeds_perc80_2 <- original_seed_words[!original_seed_words%in%perc80s_2]
removed_seeds_perc80_3 <- original_seed_words[!original_seed_words%in%perc80s_3]
removed_seeds_perc80_1;removed_seeds_perc80_2;removed_seeds_perc80_3

# comparing 70 % of original seed words ----------------
perc70s_1 <- perc70s[[1]] ; perc70s_2 <- perc70s[[2]]; perc70s_3 <- perc70s[[3]]
removed_seeds_perc70_1 <- original_seed_words[!original_seed_words%in%perc70s_1]
removed_seeds_perc70_2 <- original_seed_words[!original_seed_words%in%perc70s_2]
removed_seeds_perc70_3 <- original_seed_words[!original_seed_words%in%perc70s_3]
removed_seeds_perc70_1;removed_seeds_perc70_2;removed_seeds_perc70_3


message('First set removed at different levels')
message('90');removed_seeds_perc90_1; message('80');removed_seeds_perc80_1; message('70');removed_seeds_perc70_1
message('Second set removed at different levels')
message('90');removed_seeds_perc90_2; message('80');removed_seeds_perc80_2; message('70');removed_seeds_perc70_2
message('Third set removed at different levels')
message('90');removed_seeds_perc90_3; message('80');removed_seeds_perc80_3; message('70');removed_seeds_perc70_3

