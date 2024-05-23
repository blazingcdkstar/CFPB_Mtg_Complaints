
udf_termfreq_detail <- function(df, mytoken = "foreclosure"){
  
  df$myWord <- ifelse(regexpr(pattern = tolower(mytoken),
                              text = df$complaint.work,
                              fixed = TRUE) > 0, 1, 0)
  
 df <- df %>% 
    select(Complaint.ID, Date.received, MthYr, Company, Issue, State, 
           Sentiment.Score, myWord, Consumer.complaint.narrative) %>% 
    arrange(desc(Date.received)) %>% 
    mutate(Date.Received = as.character(Date.received)) %>% 
   select(Complaint.ID, Date.Received, MthYr, Company, Issue, State, 
          Sentiment.Score, myWord, Consumer.complaint.narrative)
 
  
  return(df)
}




udf_termfreq_bank <- function(df, mytoken = "foreclosure"){
  
  df$myWord <- ifelse(regexpr(pattern = tolower(mytoken),
                              text = df$complaint.work,
                              fixed = TRUE) > 0, 1, 0)

  df_piv <- df %>%
    select(Mth.Received, Company, myWord) %>%
    group_by(Mth.Received, Company) %>%
    summarise(Obs = n(),
              Total = sum(myWord)) %>%
    mutate(Perc.True = 100*(round(Total/Obs,3)))
  
  
  return(df_piv)
}

 

udf_termfreq_issue <- function(df, mytoken = "foreclosure"){
  
  df$myWord <- ifelse(regexpr(pattern = tolower(mytoken),
                             text = df$complaint.work,
                             fixed = TRUE) > 0, 1, 0)
  
  df_piv <- df %>%
    select(Mth.Received, Issue, myWord) %>%
    group_by(Mth.Received, Issue) %>%
    summarise(Obs = n(),
              Total = sum(myWord)) %>%
    mutate(Perc.True = 100*(round(Total/Obs,3)))
  
  
  return(df_piv)
}






  