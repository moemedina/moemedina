# Dependencies 
paquetes<-c("tidyverse","lubridate","stringr","janitor","tm","openxlsx","arrow","caret")

sapply(paquetes,
       function(x) {
         if(!x %in% rownames(installed.packages()))
           install.packages(x)
         require(x, character.only=T)})
rm(list=setdiff(ls(),c("")))
set.seed(1996)

# A lot of news, lets sample :)
data <- data.table::fread('/Users/mario/OneDrive/ITAM/4to semestre/Inv Aplicada II/Mario - Tesis/code/BETO/data/final_results_text.csv')
strat_index <- createDataPartition(data$label, p = 1, list = FALSE)
stratified_data <- data[strat_index, ]

# How many news from each label?
sample_size <- 38131*2
label_0_count <- sum(stratified_data$label == 0)
label_1_count <- sum(stratified_data$label == 1)
max_sample_size <- min(sample_size, label_0_count, label_1_count)
sample_label_0 <- stratified_data[stratified_data$label == 0, ][sample(label_0_count, max_sample_size), ]
sample_label_1 <- stratified_data[stratified_data$label == 1, ][sample(label_1_count, max_sample_size), ]
final_sample <- rbind(sample_label_0, sample_label_1)
final_sample <- final_sample[sample(nrow(final_sample)), ]
table(final_sample$label)

# Compare cleaning vs. new data
final_sample <- final_sample |> select(text, label) |>
  mutate(original_text = text)

# Data Cleansing
stop_words_1 <-data.frame(word=stopwords(kind = "spanish"))
stop_words_2 <- stop_words_1 # Without special characters such as accents.. 
stop_words_2$word <- iconv(stop_words_2$word,from="UTF-8",to="ASCII//TRANSLIT")
stop_words_2$word <- str_replace_all(stop_words_2$word, "'","") 
stop_words_2$word <- str_replace_all(stop_words_2$word, "~","")
stop_words_2$word <- str_replace_all(stop_words_2$word, "`","") 

stop_words <- bind_rows(stop_words_1,stop_words_2) %>% 
  distinct(word,.keep_all = TRUE)
rm(stop_words_1,stop_words_2)

# 1 Remove numbers, non-alphanumeric characters, double spaces, stopwords...
final_sample$text <- gsub("[^\\s]*https://[^\\s]*","", final_sample$text, perl=T) #remove urls
final_sample$text <- gsub("\\n", "", final_sample$text) # new lines

final_sample$text <- tolower(final_sample$text) # lower-case
final_sample$text <- gsub("[^\\s]*@[^\\s]*","", final_sample$text, perl=T) # remove user names 

final_sample$text <- gsub('[0-9]+', '',final_sample$text) # remove numbers 
final_sample$text <- str_replace_all(final_sample$text, "[^[:alnum:]]", " ") #swap out all non-alphanumeric characters

# tweets$text  <- gsub(paste0('\\b',stop_words$word, '\\b', # remove stopwords from list (DO WE NEED IT FOR BERT?)
#                             collapse = '|'), '', tweets$text )

final_sample$text <- str_replace_all(final_sample$text, "  ", " ") # remove double spaces
final_sample$text <- str_replace_all(final_sample$text, "  ", " ") # remove double spaces
final_sample$text <- str_replace_all(final_sample$text, "  ", " ") # remove double spaces
final_sample$text <- str_trim(final_sample$text) # remove spaces at the begginig and end of a text
final_sample$text[final_sample$text==""] <- NA # Impute NA to characters without strings
final_sample$text[which(is.na(final_sample$text))]<-"1"

# Encoding(tweets$text[2])
final_sample$text <- iconv(final_sample$text,from="UTF-8",to="ASCII//TRANSLIT")
final_sample$text <- str_replace_all(final_sample$text, "'","") # Quitar virgulillas y acentos 
final_sample$text <- str_replace_all(final_sample$text, "~","")
final_sample$text <- str_replace_all(final_sample$text, "`","") 
final_sample$text <- gsub("[^\\s]*jajaja[^\\s]*","", final_sample$text, perl=T) #remove jajaja's
final_sample$text <- gsub("[^\\s]*jiji[^\\s]*","", final_sample$text, perl=T) #remove jijiji's
final_sample$text <- gsub("[^\\s]*haha[^\\s]*","", final_sample$text, perl=T) #remove hahaha's

# Remove stop words: especificamente palabras correspondientes a los periodicos 
other_stop_words <- c("periodico","jornadamiercoles","jornadajueves","jornadalunes",
                      "jornadamartes","jornadaviernes","jornadasabado",
                      "jornada","jornadadomingo","universal","financiero")
final_sample$text <- str_replace_all(final_sample$text, paste(other_stop_words, collapse = "|"), "")
final_sample$text <- str_trim(final_sample$text) 
final_sample$text <- str_replace_all(final_sample$text, "  ", " ") # remove double spaces

# Lastly, removing single letters:
final_sample$text <- gsub(" *\\b[[:alpha:]]{1}\\b *"," ", final_sample$text, perl=T)

# Remove news with very few words or maybe very long(?)
final_sample <- final_sample %>% 
  dplyr::filter(text!="1") %>% 
  dplyr::mutate(num_car= str_count(text, "\\w+")) %>% 
  dplyr::filter(num_car>=50) %>% # Remove tweets with less that 50 words
  dplyr::filter(num_car<=1000) %>%
  dplyr::select(- num_car)

# Statistics about how many words do the news have 
## Q 3 + 1.5 ⋅ IQR (more than 1,100 words is rare)

# SAVING DATA
final_sample <- 
  final_sample %>%
  dplyr::mutate(label_str = ifelse(label == "1", "PRO", "ANTI"))

data.table::fwrite(final_sample, file = "/Users/mario/OneDrive/ITAM/4to semestre/Inv Aplicada II/Mario - Tesis/code/BETO/data/final_results_text_clean.csv")

# Assuming 'data_frame' is your data frame
write_parquet(final_sample, "/Users/mario/OneDrive/ITAM/4to semestre/Inv Aplicada II/Mario - Tesis/code/BETO/data/final_results_text_clean.parquet")


final_sample <- data.table::fread("/Users/mario/OneDrive/ITAM/4to semestre/Inv Aplicada II/Mario - Tesis/code/BETO/data/final_results_text_clean.csv")
# LESS OBSERVATIONS
final_sample1 <- 
  final_sample %>%
  dplyr::filter(label_str == "PRO") %>%
  slice_sample(n = 2500)

final_sample2 <- 
  final_sample %>%
  dplyr::filter(label_str == "ANTI") %>%
  slice_sample(n = 2500)

final_sample_bucket <- rbind(final_sample1, final_sample2)
final_sample_bucket <- final_sample_bucket[sample(nrow(final_sample_bucket)), ]

write_parquet(final_sample_bucket, "/Users/mario/OneDrive/ITAM/4to semestre/Inv Aplicada II/Mario - Tesis/code/BETO/data/5k_results_text_clean.parquet")
data.table::fwrite(final_sample_bucket, file = "/Users/mario/OneDrive/ITAM/4to semestre/Inv Aplicada II/Mario - Tesis/code/BETO/data/5k_results_text_clean.csv")
