# ---------------------------------------------------- load libraries ------------------------------------------------------ #

library(pacman)

# load data wrangling and shaping libraries
p_load(tidyverse, stringr, qdapRegex, tidyr, syuzhet, tm)



# create copy of data frame df for the sentiment analysis

text_df <- df

# copy text column to new columns for cleaning and extracting data
# create new working column, named complaint work
text_df$complaint.work <- text_df$Consumer.complaint.narrative

# capture character count with spaces
text_df$cc <- str_count(text_df$Consumer.complaint.narrative)
# capture the count of spaces in the text
text_df$wc <- str_count(rm_white(text_df$Consumer.complaint.narrative), " ")


# clean up text data

# change to lower case
text_df$complaint.work <- tolower(text_df$complaint.work)

# capture rows with blocked out text - the "X"s
text_df$count.X <- str_count(text_df$complaint.work, "x")

# remove blocked out text

blocked <- c('x', 'xx/xx/xxxx', 'xx/xx/', 'xxxx', 'xxx', 'xx')

for(i in seq_along(blocked)){
  
  mytext = blocked[i]
  text_df$complaint.work <- gsub(pattern = mytext,
                                 replacement = '',
                                 x = text_df$complaint.work,
                                 fixed = TRUE)
  
}


# remove numbers
text_df$complaint.work <- gsub(pattern = "[[:digit:]]", replacement = "", x = text_df$complaint.work)

# remove punctuation
text_df$complaint.work <- gsub(pattern = "[[:punct:]]", replacement = "", x = text_df$complaint.work)

text_df$complaint.work <- gsub(pattern = "[[:cntrl:]]", replacement = "", x = text_df$complaint.work)

# remove extra white space
text_df$complaint.work <- str_squish(text_df$complaint.work)


# for server part

text_df_s <- text_df 


# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text_df_s$complaint.work, method="syuzhet")

# add syuzhet_vector to dataframe
text_df_s <- cbind(text_df_s, syuzhet_vector)

# create column, myWord, to identify if the word is in the complaint with a 1 or not with a 0. The default word is 'foreclosure'.
text_df_s$myWord <- ifelse(regexpr(pattern = "foreclosure",
                                  text = text_df_s$complaint.work) > 0, 1, 0)

# format date
text_df_s$Mth.Received <- floor_date(df$Date.received, unit = 'month')

# join the syuzehet vector to the main text by complaint id
df <- df %>% 
  left_join(select(text_df_s, Complaint.ID, syuzhet_vector),
            by = "Complaint.ID")
# change the column names
names(df)[which(names(df) == "syuzhet_vector")] <- "Sentiment.Score"
names(text_df_s)[which(names(text_df_s) == "syuzhet_vector")] <- "Sentiment.Score"

# filter dataset for companies in company list
text_df_s <- text_df_s %>% filter(Company %in% company_list)


termfreq = text_df_s
