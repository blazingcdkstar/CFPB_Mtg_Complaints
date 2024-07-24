


library(shiny)
library(shinyWidgets)
library(shinyjs)
source("data.R")
source("sentiment.R")
source("word_search.R")





# Define UI for application that draws a histogram

ui <- 
  fluidPage(
  
  
  
    #----------------------- UI Sidebar Panel --------------------------------------
  # 2 picker inputs, and 1 date range input
  theme = bslib::bs_theme(bootswatch = 'flatly'),
  
  titlePanel("CFPB Mortgage Complaint Data"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      id = "sidebar",
      width = 3,
      
      radioGroupButtons(inputId = "grouping_var",
                        label = "Choose Grouping Variable",
                        choices = c("Bank", "Issue")),
      
      pickerInput("states", label = "Select State", 
                  choices = state_list,
                  multiple = TRUE,
                  options = pickerOptions(
                  actionsBox = TRUE,
                  title = "Please select a state")),
   
      uiOutput("banks"),
      
      uiOutput("issues"),
      
      dateRangeInput("date_range", "Date Range: ",
                start = date_range_start,
                end = date_range_end),
      
      textInput("token", paste("Word Search: Enter a word or phase below.",
                               sep = ""),
                               value = "foreclosure"),
      
  
      
      

     
),
#----------------------- UI Main Panel --------------------------------------
 
 mainPanel(
   tabsetPanel(
     
#----------------------- UI Tab Panel 3: Bubble Map  --------------------------------------
     
tabPanel("App Home",
         
         
              
        # add a space break 
              
        tags$h6(" "),
        
        tags$div(class = "jumbotron text-center", 
                 style = "margin-bottom:0px;margin-top:0px",
                 tags$h2(class = 'jumbotron-heading',
                         stye = 'margin-bottom:0px;margin-top:0px',
                         'CFPB Mortgage Complaints'),
                 p("Learn about customer complaints filed with the CFPB.")
        ),
        
        tags$h3("User Instructions"),
        tags$h6(" "),
        tags$li("Select either bank or issue for grouping variable. The default selection is bank."),
        tags$h6(" "),
        tags$li("Select State(s), Bank(s) and Issue(s) from the drop down lists. Charts will not appear until 1 of each are selected."),
        tags$h6(" "),
        tags$li(paste("Modify Date Range as needed to limit scope. Data is pulled for ", mindate_mth,' ',mindate_date,', ', mindate_year,', ', " forward.", sep = '')),
        tags$h6(" "),
        tags$li("Word Search Tab: Enter a word or phrase in the text box. The default word is set to 'FORECLOSURE'."),
        tags$h6(" "),
        tags$li("You can also select by clicking on grouping in the plot legend, and you can download the plots."),
        
        
        tags$h3("Tab Information"),
        tags$h6(" "),
        tags$li("Monthly Volume: Monthly volume of complaints by received date."),
        tags$h6(" "),
        tags$li("Complaints Mapped: Geographical distribution of complaints by zipcode."),
        tags$h6(" "),
        tags$li("Monthly Sentiment: Median sentiment score by month."),
        tags$h6(" "),
        tags$li("Word Search: Monthly percentage of complaints containing the entered word or phrase."),
        tags$h6(" "),
        tags$li("Get Data: The 5 most recent complaints given the selection will show in the app screen. Click the 'Download Detail.csv'
                button to download entire selected data set."),
        
        tags$h3("Data Source"),
        tags$h6(" "),
        tags$li("CFPB Data: https://www.consumerfinance.gov/data-research/consumer-complaints/search"),
        tags$a(href="https://www.consumerfinance.gov/data-research/consumer-complaints/search", 
               "Click here!",
               target="_blank"),
        tags$h6(" "),
        tags$li("I am selecting for mortgages and for complaints that include a customer narrative."),
        tags$h6(" "),
        tags$li(paste("Data is limited to banks that have had at least 3 complaints for 1 month from ", mindate_mth," ",mindate_date," ", mindate_year, " forward.", sep = "")),
        tags$h6(" "),
        tags$li("Sentiment scoring is based on the syuzhet method."),
        tags$h6(" "),
        tags$li("The column 'myWord' shows a 1 when the entered word or phrase exists in the consumer complaint narrative.")
        
        
        
        
        
        
              
     
              
              
  ),
     
     
#----------------------- UI Tab Panel 2: Summary level and Plot  --------------------------------------
 
     # In this tab the user can interact with the input selections, and a chart will be presented based on those selections
     # The user will be able to download summary data in csv file and the resulting plot
     
tabPanel("Monthly Volume", 
              # add a space break
              tags$h6(" "),
 
              # add a space break 
              tags$h3(" "), 
              
              
              # add an empty line for spacing
              tags$h3(" "),
              
              # output the summary plot
              plotlyOutput("sum_plot")
         
         ), 
 

     
 #----------------------- UI Tab Panel 3: Bubble Map  --------------------------------------
 
tabPanel("Complaints Mapped",
               
            # add a space break 
            
            tags$h6(" "),
         
            plotlyOutput("bubble_map")
          
           
      ),
 
 
 #----------------------- UI Tab Panel 4: Sentiment Plot  --------------------------------------
 
 tabPanel("Monthly Sentiment",
          
          # add a space break 
          
          tags$h6(" "),
          
          plotlyOutput("sent_plot")
          
          
 ),
 
 
 #----------------------- UI Tab Panel 5: Word Search -----------------------------------------
 
 tabPanel("Word Search",
          
          # add a space break 
          
          tags$h6(" "),
          
          #textInput("token", "Enter a word or phase below.", value = "foreclosure"),
          
          plotlyOutput("termfreqplot")
          
          
          
         
          
 ),


#----------------------- UI Tab Panel 6: Detail level --------------------------------------
tabPanel("Get Data",
         tags$h6("Detail for 5 most recent complaints for selected population."),
         tags$h6(" "),
         tags$h6("Select Download Detail for export of complete result set for selected population."), 
         downloadButton(outputId = "downloadData",
                        label = "Download Detail.csv",
         ),
         
         #tableOutput('table'),
         
         tableOutput('table2')
),


id = "tabset"
   ), 
id="main"
 
 

)
)
)
 
# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
  
# ------------------------------- get picker input lists ------------------------------

  output$banks <- renderUI({
    
    choices = sort(unique(df$Company[which(df$State %in% input$states)]))
    
    
    pickerInput('banks', 'Select Bank', 
                choices = choices, 
                multiple = TRUE,
                options = pickerOptions(
                actionsBox = TRUE,
                `live-search` = TRUE,
                title = "Please select a bank"))
  })
  
  
  
  output$issues <- renderUI({
    
    choices = sort(unique(df$Issue[which(df$State %in% input$states &
                               df$Company %in% input$banks)]))
  
    
    
    pickerInput('issues', 'Select Issue', 
                choices = choices, 
                multiple = TRUE,
                options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select an issue"
                ))
  })
  
  
 #----------------------- get data results for input selections --------------------------------------
  
  data <- reactive({
    df %>%
      filter(Company %in% .env$input$banks &
               Issue %in% .env$input$issues &
               State %in% .env$input$states &
               Date.received >= .env$input$date_range[1] &
               Date.received <= .env$input$date_range[2]) %>%
      select(Date.received, Complaint.ID, MthYr, Qrtr, Company, Product, Sub.product, Issue,  
             State, Consumer.complaint.narrative, Consumer.disputed., Timely.response., lat, lng, Sentiment.Score) %>% 
      arrange(desc(Date.received)) %>% 
      mutate(Date.Received = as.character(Date.received)) %>% 
      select(Date.Received, Complaint.ID, MthYr, Qrtr, Company, Product, Sub.product, Issue, 
             State, Consumer.complaint.narrative, Consumer.disputed., Timely.response., lat, lng, Sentiment.Score)
    
   
    })
  
  
  
  
  
  
  # get data for sentiment analysis
  
  sent_sum <- reactive({
    text_df_s %>% 
      filter(Company %in% .env$input$banks &
               Issue %in% .env$input$issues &
               State %in% .env$input$states &
               Date.received >= .env$input$date_range[1] &
               Date.received <= .env$input$date_range[2]) %>%
      
      select(MthYr, Sentiment.Score) %>% 
      group_by(MthYr) %>% 
      summarise(MedianSent = round(median(Sentiment.Score),2),
                AverageSent = round(mean(Sentiment.Score),2),
                MinSent = min(Sentiment.Score),
                MaxSent = max(Sentiment.Score),
                Obs = n())
    
    
    
  })
  
  
  sent_sum_bank <- reactive({
    text_df_s %>% 
      filter(Company %in% .env$input$banks &
               Issue %in% .env$input$issues &
               State %in% .env$input$states &
               Date.received >= .env$input$date_range[1] &
               Date.received <= .env$input$date_range[2]) %>%
      
      select(MthYr, Company, Sentiment.Score) %>% 
      group_by(MthYr, Company) %>% 
      summarise(MedianSent = round(median(Sentiment.Score),2),
                Obs = n())
    
    
    
  })
  
  
  sent_sum_issue <- reactive({
    text_df_s %>% 
      filter(Company %in% .env$input$banks &
               Issue %in% .env$input$issues &
               State %in% .env$input$states &
               Date.received >= .env$input$date_range[1] &
               Date.received <= .env$input$date_range[2]) %>%
      
      select(MthYr, Issue, Sentiment.Score) %>% 
      group_by(MthYr, Issue) %>% 
      summarise(MedianSent = round(median(Sentiment.Score),2),
                Obs = n())
    
    
    
  })
  
  
  
  # get data for terms
  
 term_freq <- reactive({
   text_df_s %>% 
     filter(Company %in% .env$input$banks &
              Issue %in% .env$input$issues &
              State %in% .env$input$states &
              Date.received >= .env$input$date_range[1] &
              Date.received <= .env$input$date_range[2])
     
   
 })
 
 
 
 
 
 
 term_freq_detail <- reactive({
    udf_termfreq_detail(df = term_freq(), mytoken = input$token)
   
 })
 
 
  term_freq_bank <- reactive({
   if(input$grouping_var == "Bank") {
   udf_termfreq_bank(df = term_freq(), mytoken = input$token)
   }
 })

 
 term_freq_issue <- reactive({
   if(input$grouping_var == "Issue") {
   udf_termfreq_issue(df = term_freq(), mytoken = input$token)
   }
 })
 



  
  #----------------- get summary data results for input selections of bank v issue -----------------

  data_piv_issue <- reactive({
    if(input$grouping_var == "Issue") {
    data() %>% 
    select(MthYr, Qrtr, Issue) %>% 
    group_by(MthYr, Qrtr, Issue) %>% 
    summarise(Complaint_Count = n()) 
    }
      
  })

  
    data_piv_bank <- reactive({
      if(input$grouping_var == "Bank") {
      data() %>% 
        select(MthYr, Qrtr, Company) %>% 
        group_by(MthYr, Qrtr, Company) %>% 
        summarise(Complaint_Count = n()) 
      }
      
    })
    
  
    
    map_piv_issue <- reactive({
      if(input$grouping_var == "Issue") {
      data() %>% 
      select(Issue, lat, lng, Complaint.ID) %>% 
      group_by(Issue, lat, lng) %>% 
      summarise(Complaint_Count = n(),
                Max_ComplaintID = max(Complaint.ID))
      }
    
      
    })
    
    
    map_piv_bank <- reactive({
      if(input$grouping_var == "Bank") {
      data() %>% 
        select(Company, lat, lng, Complaint.ID) %>% 
        group_by(Company, lat, lng) %>% 
        summarise(Complaint_Count = n(),
                  Max_ComplaintID = max(Complaint.ID))
      }
      
    })
    
    
    
      
    
    
 
   
  #---------------------------  Create Plots  -----------------------------------

  # create summary plot by issue
    my_plot_issue <- reactive({
      if(input$grouping_var == "Issue") {
        plot_ly(data_piv_issue(),
                x = ~MthYr,
                y = ~Complaint_Count,
                type = 'scatter',
                mode = 'markers',
                color = ~Issue,
                colors = c("#880E4F","#F06292","#1A237E", "#7986CB", "#004D40", "#4DB6AC"),
                line = list(shape = 'linear'),
                hoverinfo = 'text',
                text = ~paste("Month Received: ", data_piv_issue()$MthYr, "<br />",
                              "Issue Category: ", data_piv_issue()$Issue, "<br />",
                              "Complaint Count: ", data_piv_issue()$Complaint_Count, sep = "")) %>% 
          config(modeBarButtonsToRemove = c("lasso2d",
                                            "pan2d",
                                            "select2d",
                                            "hoverClosestGeo",
                                            "autoscale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian"))%>% 
          layout(title = 'Monthly Complaint Volume by Issue',
                 xaxis = list(title = 'Month Received'), 
                 yaxis = list(title = 'Complaint Volume'), 
                 plot_bgcolor = "E7E8E9",
                 legend = list(title=list(text='<b> Issue Category </b>')))
      }
      
    }) 
    

    
   # create summary plot by bank
    my_plot_bank <- reactive({
      if(input$grouping_var == "Bank") {
        plot_ly(data_piv_bank(),
                x = ~MthYr,
                y = ~Complaint_Count,
                type = 'scatter',
                mode = 'markers',
                color = ~Company,
                colors = 'Dark2',
                line = list(shape = 'linear'),
                hoverinfo = 'text',
                text = ~paste("Month Received: ", data_piv_bank()$MthYr, "<br />",
                              "Bank Name: ", data_piv_bank()$Company, "<br />",
                              "Complaint Count: ", data_piv_bank()$Complaint_Count, sep = "")) %>% 
          config(modeBarButtonsToRemove = c("lasso2d",
                                            "pan2d",
                                            "select2d",
                                            "hoverClosestGeo",
                                            "autoscale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian")) %>%
          layout(title = 'Monthly Complaint Volume by Bank',
                 xaxis = list(title = 'Month Received'), 
                 yaxis = list(title = 'Complaint Volume'), 
                 plot_bgcolor = "E7E8E9",
                 legend = list(title=list(text='<b> Bank Name </b>')))
          
          
      }
      
    }) 
  
 
    # create bubble map by bank   
    
  map_plot_bank <- reactive({
    if(input$grouping_var == "Bank") {
      plot_geo(map_piv_bank(), locationmode = 'USA-states', sizes = c(1, 250)) %>% 
        add_markers(x = ~lng,
                    y = ~lat,
                    color = ~Company,
                    size = ~Complaint_Count*2,
                    colors = 'Dark2',
                    hoverinfo = 'text',
                    text = ~paste("Bank Name: ", map_piv_bank()$Company, "<br />",
                                  "Complaint Count: ", map_piv_bank()$Complaint_Count, "<br />",
                                  "Most Recent Complaint ID: ", map_piv_bank()$Max_ComplaintID, sep = "")) %>% 
        config(modeBarButtonsToRemove = c("lasso2d",
                                          "pan2d",
                                          "select2d",
                                          "hoverClosestGeo")) %>%
        layout(title = "Distribution of Complaint Volume by Bank",
               legend = list(orientation = "v", y = .5, x = 1,
                             title=list(text='<b> Bank Name </b>')),
               geo = g)
      
    }
  })
  
  
  
  # create bubble map by issue  
  
  map_plot_issue <- reactive({
   if(input$grouping_var == "Issue") {
     plot_geo(map_piv_issue(), locationmode = 'USA-states', sizes = c(1, 250)) %>%
       add_markers(x = ~lng,
                   y = ~lat,
                   color = ~Issue,
                   size = ~Complaint_Count,
                   colors = c("#880E4F","#F06292","#1A237E", "#7986CB", "#004D40", "#4DB6AC"),
                   hoverinfo = 'text',
                   text = ~paste("Issue Category: ", map_piv_issue()$Issue, "<br />",
                                 "Complaint Count: ", map_piv_issue()$Complaint_Count,"<br />",
                                 "Most Recent Complaint ID: ", map_piv_issue()$Max_ComplaintID, sep = "")) %>%
       config(modeBarButtonsToRemove = c("lasso2d",
                                         "pan2d",
                                         "select2d",
                                         "hoverClosestGeo")) %>%
       layout(title = "Distribution of Complaint Volume by Issue",
              legend = list(orientation = "v", y = .5, x = 1,
                            title=list(text='<b> Issue Category </b>')),
              geo = g)
     
   }
   
 })
  
  
  
 
 
 sentiment_bank_plot <- reactive({
   if(input$grouping_var == "Bank") {  
   plot_ly(sent_sum_bank(),
           x = ~MthYr,
           y = ~MedianSent,
           type = 'scatter',
           mode = 'markers',
           color = ~Company,
           colors = 'Dark2',
           line = list(shape = 'linear'),
           hoverinfo = 'text',
           text = ~paste("Month Received: ", sent_sum_bank()$MthYr, "<br />",
                         "Bank Name: ", sent_sum_bank()$Company, "<br />",
                         "Median Sentiment: ", sent_sum_bank()$MedianSent, "<br />",
                         "Complaint Count: ", sent_sum_bank()$Obs, sep = "")) %>% 
     config(modeBarButtonsToRemove = c("lasso2d",
                                       "pan2d",
                                       "select2d",
                                       "hoverClosestGeo",
                                       "autoscale2d",
                                       "hoverClosestCartesian",
                                       "hoverCompareCartesian")) %>% 
       layout(title = 'Monthly Median Sentiment by Bank',
              xaxis = list(title = 'Month Received'), 
              yaxis = list(title = 'Median Sentment'), 
              plot_bgcolor = "E7E8E9",
              legend = list(title=list(text='<b> Bank Name </b>')))
     
   }
   
   
 })
 
 
 sentiment_issue_plot <- reactive({
   if(input$grouping_var == "Issue") {
     
      plot_ly(sent_sum_issue(),
           x = ~MthYr,
           y = ~MedianSent,
           type = 'scatter',
           mode = 'markers',
           color = ~Issue,
           colors = c("#880E4F","#F06292","#1A237E", "#7986CB", "#004D40", "#4DB6AC"),
           line = list(shape = 'linear'),
           hoverinfo = 'text',
           text = ~paste("Month Received: ", sent_sum_issue()$MthYr, "<br />",
                         "Issue Category: ", sent_sum_issue()$Issue, "<br />",
                         "Median Sentiment: ", sent_sum_issue()$MedianSent, "<br />",
                         "Complaint Count: ", sent_sum_issue()$Obs, sep = "")) %>% 
     config(modeBarButtonsToRemove = c("lasso2d",
                                       "pan2d",
                                       "select2d",
                                       "hoverClosestGeo",
                                       "autoscale2d",
                                       "hoverClosestCartesian",
                                       "hoverCompareCartesian")) %>% 
       layout(title = 'Monthly Median Sentiment by Issue',
              xaxis = list(title = 'Month Received'), 
              yaxis = list(title = 'Median Sentment'), 
              plot_bgcolor = "E7E8E9",
              legend = list(title=list(text='<b> Issue Category </b>')))
  
   }
   
})
 
 
 
 
 
 
 
 
 term_freq_bank_plot <- reactive({
   if(input$grouping_var == "Bank") { 
   plot_ly(term_freq_bank(),
           x = ~Mth.Received,
           y = ~Perc.True,
           type = 'scatter',
           mode = 'markers',
           color = ~Company,
           colors = 'Dark2',
           line = list(shape = 'linear'),
           hoverinfo = 'text',
           text = ~paste("Month Received: ", term_freq_bank()$Mth.Received, "<br />",
                         "Bank Name: ", term_freq_bank()$Company, "<br />",
                         "Percent of Complaints Containing ", toupper(input$token), ": ", 
                          term_freq_bank()$Perc.True," %", "<br />",
                         "Complaint Count: ", term_freq_bank()$Obs, sep = "")) %>% 
     config(modeBarButtonsToRemove = c("lasso2d",
                                       "pan2d",
                                       "select2d",
                                       "hoverClosestGeo",
                                       "autoscale2d",
                                       "hoverClosestCartesian",
                                       "hoverCompareCartesian")) %>% 
       layout(title = paste("Percentage of Complaints Containing Word or Phrase: ", toupper(input$token), sep = ""),
              xaxis = list(title = 'Month Received'), 
              yaxis = list(title = 'Percentage'), 
              plot_bgcolor = "E7E8E9",
              legend = list(title=list(text='<b> Bank Name </b>'))) 
     
   }
   
 })
 
 
 term_freq_issue_plot <- reactive({
   if(input$grouping_var == "Issue") { 
   plot_ly(term_freq_issue(),
           x = ~Mth.Received,
           y = ~Perc.True,
           type = 'scatter',
           mode = 'markers',
           color = ~Issue,
           colors = c("#880E4F","#F06292","#1A237E", "#7986CB", "#004D40", "#4DB6AC"),
           line = list(shape = 'linear'),
           hoverinfo = 'text',
           text = ~paste("Month Received: ", term_freq_issue()$Mth.Received, "<br />",
                         "Issue Category: ", term_freq_issue()$Issue, "<br />",
                         "Percent of Complaints Containing Word: ", term_freq_issue()$Perc.True," %", "<br />",
                         "Complaint Count: ", term_freq_issue()$Obs, sep = "")) %>% 
     config(modeBarButtonsToRemove = c("lasso2d",
                                       "pan2d",
                                       "select2d",
                                       "hoverClosestGeo",
                                       "autoscale2d",
                                       "hoverClosestCartesian",
                                       "hoverCompareCartesian")) %>% 
       layout(title = paste("Percentage of Complaints Containing Word or Phrase: ", toupper(input$token), sep = ""),
              xaxis = list(title = 'Month Received'), 
              yaxis = list(title = 'Percentage'), 
              plot_bgcolor = "E7E8E9",
              legend = list(title=list(text='<b> Issue Category </b>'))) 
   }
   
 })
 

 


  #---------------------------  Create download handlers  -----------------------------------
  
  # download handler for detail level data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(),'_detail_dataset.csv',sep = '')
    },
    content = function(file) {
      write.csv(term_freq_detail(), file,row.names = FALSE)
    }
  )
  
 
    #---------------------------  Render Outputs  -----------------------------------

   # render summary plot
  output$sum_plot <- renderPlotly({
    req(input$banks)
    req(input$issues)
    if(input$grouping_var == "Issue"){
      my_plot_issue()
    }
    else if(input$grouping_var == "Bank"){
      my_plot_bank()
    }  
  })
  
  
 # # render bubble map
  output$bubble_map <- renderPlotly({
    req(input$banks)
    req(input$issues)
    if(input$grouping_var == "Issue"){
      map_plot_issue()
    }
    else if(input$grouping_var == "Bank"){
    map_plot_bank()
    }
   
  })
  
  
# render sentiment plot
  output$sent_plot <- renderPlotly({
    req(input$banks)
    req(input$issues)
    if(input$grouping_var == "Issue"){
      sentiment_issue_plot()
    }
    else if(input$grouping_var == "Bank"){
      sentiment_bank_plot()
    }  
  })
  
  
  # render term frequency
  output$termfreqplot <- renderPlotly({
    req(input$banks)
    req(input$issues)
    if(input$grouping_var == "Issue"){
      term_freq_issue_plot()
    }
    else if(input$grouping_var == "Bank"){
      term_freq_bank_plot()
    }  
  })
  
  
  

# render detail table
output$table <- renderTable(
  head(detail_data(), n = 5), rownames = FALSE)


# render detail table
output$table2 <- renderTable(
  head(term_freq_detail(), n = 5), rownames = FALSE)



#render detail table
output$wf_table <- renderTable(
  head(text_df_s_tf(), n = 5), rownames = FALSE)


#render detail table
output$wf_table_new <- renderTable(
  head(text_df_s_tf_new(), n = 5), rownames = FALSE)

#render detail table
output$wf_table_piv <- renderTable(
  head(temp_piv(), n = 5), rownames = FALSE)


output$text <- renderText({ myword() })




}

# Run the application 
shinyApp(ui = ui, server = server)
