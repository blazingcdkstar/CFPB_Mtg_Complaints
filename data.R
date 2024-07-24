
# ---------------------------------------------------- load libraries ------------------------------------------------------ #
library(pacman)
p_load(dplyr, ggplot2, lubridate, plotly, zipcodeR)

# ---------------------------------------------------- set variables  ------------------------------------------------------- #

# create path variable
mypath <- "C:/Users/kelle/Desktop/My Stuff/R Projects/Shiny/Complaints SbS/Data"


# create user defined functions
# add udf_cleanup to ggplot2 maps to have a clean look
udf_cleanup <- theme(panel.grid.major = element_blank(),                # remove major grid lines
                 panel.grid.minor = element_blank(),                    # remove minor grid lines
                 panel.background = element_blank(),                    # have blank background
                 axis.line.x = element_line(color = "black"),           # add black line at the x axis 
                 axis.line.y = element_line(color = "black"),           # add black line at the y axis
                 legend.key = element_rect(fill = "white"),             # fill the legend rectangle with white color
                 text = element_text(size = 10))                        # use font size of 10


# create geography for map chart
g <- list(
  scope = 'usa',
  resolution = 150,
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)


# ---------------------------------------------------- import data  ------------------------------------------------------- #
# small data frame for testing or if working offline.
 # setwd(mypath)
 # df <- read.csv("Complaints_Small.csv")

#import live cfpb data
# set min/max date range for import
mindate <- Sys.Date() - months(18)
maxdate <- Sys.Date()

mindate_mth <- as.character(month(mindate, label = TRUE, abbr = FALSE))
mindate_date <- as.character(day(mindate))
mindate_year <- as.character(year(mindate))

# create the url. this is split into 3 sections to capture the date critier in the url.

starturl <- "https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/?date_received_max="
midurl <- "&date_received_min="
endurl <- "&field=all&format=csv&has_narrative=true&no_aggs=true&product=Mortgage&size=11282&sort=created_date_desc"

# combine the three sections to form the total url
# for example, the start url is pasted with the maxdate, to get the "date_received_max=maxdate"
# midurl is pasted to mindate to get the date_received_min=mindate

myurl <- paste(starturl,maxdate,midurl,mindate,endurl,sep = "")

# read in the data using the url into a dataframe called df
df <- read.csv(myurl)

# import zipcode data to get longitude and latitude for the map
zip <- geocode_zip(df$ZIP.code)

# join zipcode data to df
df <- df %>% 
  left_join(zip, by = c("ZIP.code" = "zipcode"))



# ---------------------------------------------------- format data  ------------------------------------------------------- #

# format dates
df$Date.received <- mdy(df$Date.received)
df$MthYr <- format_ISO8601(df$Date.received, precision = "ym")
df$Qrtr <- quarter(df$Date.received, with_year = TRUE)
df$Date.sent.to.company <- mdy(df$Date.sent.to.company)


# create list of unique credit reporting categories to callapse down to "Credit Reporting"
cred_rpt <- c("Credit monitoring or identity theft protection services", "Unable to get your credit report or credit score",
              "Improper use of your report", "Incorrect information on your report",
              "Problem with a credit reporting company's investigation into an existing problem")

# create a copy of the Issue column
df$Issue.Original <- df$Issue

# create collapsed and more readable category descriptions 
df$Issue <- ifelse(df$Issue.Original %in% cred_rpt, "Credit Reporting", 
                   ifelse(df$Issue.Original == "Applying for a mortgage or refinancing an existing mortgage",
                          "Applying or refinancing",
                          ifelse(df$Issue.Original == "Problem with fraud alerts or security freezes",
                                 "Fraud alerts or security freezes",
                          df$Issue.Original)))


# ---------------------------------------------------- Dashboard List Options  ------------------------------------------------------- #

# limit dashboard to companies with more than 2 complaints

# identify the overall count for each company by month
df_limit <- df %>% 
  select(MthYr, Company) %>%
  group_by(MthYr, Company) %>% 
  summarise(Company_Count = n())

# obtain the monthly average count by company
# and filter for those with a monthly average count greater than 2
df_limit_avg <- df_limit %>% 
  ungroup() %>% 
  select(Company, Company_Count) %>% 
  group_by(Company) %>% 
  summarise(Mthly.Avg = mean(Company_Count)) %>% 
  filter(Mthly.Avg > 2)
  
# create company list of unique company names in the df_limit_avg dataframe
# there are a lot of companies with just 1 or 2 complaints, this excludes those companies from the dashboard

df <- df %>% 
  filter(Company %in% df_limit_avg$Company)

company_list <- sort(unique(df_limit_avg$Company))

# create list of issues
issue_list <- sort(unique(df$Issue))


# create state list
state_list <- sort(unique(df$State))

# create date range variables for starting date and ending date
date_range_start <- min(df$Date.received)
date_range_end <- max(df$Date.received)




