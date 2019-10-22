#Project: Construction of a shiny dashboard for Car Accessories Configurator Clicking Data
#Author: Sandra Schuppert


#Step_1: Loading of Accessories Configurator (AC) Data (5 csv files)

#The available data shows clicking numbers for single Car Accessories Products by country, product cateogory and
#vehicle class.

library(readr)
library(dplyr)
library(ggplot2)
library(treemapify)
df1 <- read.csv("E:\\Dropbox\\Sandra\\Uni\\Weiterbildung Business Analytics\\10_Programming Languages for Data Science\\Accessories Configurator\\DataScience_18_07-09.csv", 
                na.strings=c("", "not_set"))
df2 <- read.csv("E:\\Dropbox\\Sandra\\Uni\\Weiterbildung Business Analytics\\10_Programming Languages for Data Science\\Accessories Configurator\\DataScience_18_10-12.csv", 
                na.strings=c("", "not_set"))
df3 <- read.csv("E:\\Dropbox\\Sandra\\Uni\\Weiterbildung Business Analytics\\10_Programming Languages for Data Science\\Accessories Configurator\\DataScience_19_01-03.csv", 
                na.strings=c("", "not_set"))
df4 <- read.csv("E:\\Dropbox\\Sandra\\Uni\\Weiterbildung Business Analytics\\10_Programming Languages for Data Science\\Accessories Configurator\\DataScience_19_04.csv", 
                na.strings=c("", "not_set"))
df5 <- read.csv("E:\\Dropbox\\Sandra\\Uni\\Weiterbildung Business Analytics\\10_Programming Languages for Data Science\\Accessories Configurator\\DataScience_19_04.csv", 
                na.strings=c("", "not_set"))

#Combining of 5 csv files to one dataframe
df <- rbind(df1, df2, df3, df4, df5)

#Show dataframe
library(tidyverse)
glimpse(df)

#Data contains 7 columns

#Hour: Month/Day/Year/Hour in which customer was active in AC
#GeoSegmentation Countries: Origin of customer
#Unique ID: ID containing vehicle model and motorisation
#Product ID (v7)(evar): Part number of Accessories product
#Unique visitors: Number of clicks
#Product Printouts: Total (event1): Number of printouts


#Step_2: Data adjustments (Delete, Rename, NA-values)

#Columns "Product Printouts: Total (event1)", "Unique visitors" "Unique ID" are not used for the dashboard and therefore will be delete
df$Product.Printouts..Total..event1. <- NULL
#df$Unique.ID <- NULL

#Rename columns
df <- df %>% 
    rename(Product_ID = Product.ID..v7...evar7., Country = GeoSegmentation.Countries)

#Check which columns have missing values
sum(is.na(df$Country))
sum(is.na(df$Vehicle.Class))
sum(is.na(df$Unique.ID))
sum(is.na(df$Product_ID))
sum(is.na(df$Unique.Visitors))
sum(is.na(df$Hour))


#Remove all rows with NA-values
df <- na.omit(df)

#After removing all missing values the dataframe consists of 1.836.998 rows and 6 columns.

#Step_3: Adding further columns from second dataframe "SNR-Kategorien"

#Since the part number for the Accessories products are not meaningful we will add a column "product category" by
#merging the dataframe with the excel file "SNR-Kategorien" which will be assigned as "cats".

#Load excel file "SNR-Kategorien"
install.packages("readxl")
library(readxl)
cats <- read_excel("E:\\Dropbox\\Sandra\\Uni\\Weiterbildung Business Analytics\\10_Programming Languages for Data Science\\Accessories Configurator\\SNR-Kategorien.xlsx")

#Left join of df and cats on Product_ID (column Product_ID in df is changed to character before the join)
df$Product_ID <- as.character(df$Product_ID)
ac <- left_join(df, cats, by="Product_ID")

#Now that the dataframe ist complete we will start to create the dashboard as shinyapp.

#Step_4: ShinyApp Creation

install.packages("googleVis")
library(googleVis)
library(shiny)
install.packages("shinyWidgets")
install.packages("treemapify")
library(shinyWidgets)

#The page should be split into a sidepanel in which the user can select specific variables and in the mainpanel
#holding the plots and tables.
#Since we only have text-values to select, we will only use selectInputs which will create a drop down menu.
#The user can choose the Country, the Vehicle Class and the Product category. The plots created in the next step
#are supposed to change according to the characteristic chosen by the user.
#The dashboard shall include the following plots:
#1. Number of clicks per division in a treemap plot (filter option is "Country")
#2. Top 5 products in a table (filter options are country and Vehicle Class)
#3. Number of clicks per product category in a bar plot (filter options are "Country" and "Vehicle Class")
#4. Number of clicks per product category in a country map (filter option is "Product category")
#We create two tabs: one for the first three graphs/tables called "Tables & Graphs" and one for the map called "Map".

#Step_4.1: Create UI (user interface)
#In the first step we create the input options and define the available choices. Further we define the output to be
#a table Output, a plot Output or html Output.

ui <- fluidPage(titlePanel("Car Accessories Configurator - Clicking Data"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("CountryInput", "Country:",
                                choices=unique(ac$Country %>%sort),
                                selected = "germany"),
                    selectInput("VehicleInput", "Vehicle Class:",
                                choices=unique(ac$Vehicle.Class %>%sort)),
                    selectInput("CategoryInput", "Product Category:",
                                choices=unique(ac$Category %>%sort))
                    ),
                  mainPanel(
                    tabsetPanel(type="tabs",
                                tabPanel("Tables & Graphs", fluidRow(column(7, plotOutput("bar1")),
                                                  column(5, tableOutput("table1"))), 
                                         fluidRow(column(12,plotOutput("bar2"))
                                         )),
                                tabPanel("Map", htmlOutput("gVis")))
                    
                    )
                )
)

#Step_4.2: Building the outputs
#In the second step we specify the output and link it to the input options.
#The first output should be the treemap which we named "bar1" in the output section of the UI. Therefore we will
#assign bar1 to the output. In order to plot the treemap showing the number of clicks per division with a filtering
#option for countries we will select the variables Country, Division and Unique.Visitors from the dataframe ac.
#Further we will filter for country in which we will link to the selectInput created in the UI. To show the aggre-
#gated clicks per country we will summarize by the column Unique.Visitors column and assign the name "Visitor_sum".
#This variable will define the division size in the treemap. Therefore we choose area=Visitor_sum in the treemap
#specification. By choosing fill=Division we make sure that the divisions will be sepearted by color.

#The second output "table1" will include a table showing the Top 5 products by name. The filter options are country
#vehicle class. We filter for the respective variables in the dataframe ac, group the data by name and summarize by
#the sum of Unique.Visitors. To only show the Top 5 products we order descendingly and select top_n(5).

#The third output "bar2" shall plot the number of clicks by product group in a bar plot. In this case the filter
#options are Country and Vehicle Class. The variables Country, Vehicle.Class, Category and Unique.Visitors are
#selected and grouped by Country and Category. The sum of Unique.Visitors is calculated and assigned to the name
#"Clicks" which is also going to be shown in the bar plot. The color is adjusted to dodgerblue1.

#The last output is created with the googleVis package and shows a world map in which the number of clicks is shown 
#with filter option "Product category". The countries which have high click rates for the chosen product group will
#be shown in green, the countries with lower clicking rates on the chosen product group will be shown in grey. In
#order to plot this we select Country, Category and Unique.Visitors from the dataframe, filter by Category and
#summarize the number of clicks by calculating the sum of column Unique.visitors as "Clicks2".In the gvisGeoChart specification
#we choose the column Country als location variable (locationvar) and the column "Clicks2" als color variable
#(colorvar).

#In the last step we run the ShinyApp.

server <- function(input, output) {output$bar1 <- renderPlot({
  ac_country <- 
    ac %>%
    select(Country, Division, Unique.Visitors)%>%
    filter(Country == input$CountryInput)
  ac_country2 <-
    ac_country%>%
    group_by(Country, Division)%>%
    summarize(Visitor_sum = sum(Unique.Visitors))
  ggplot(ac_country2, aes(area = Visitor_sum, fill = Division)) +
    geom_treemap()
}, height=200, width=300)
output$table1 <- renderTable({
  ac_Top5 <-
    ac%>%
    filter(Country == input$CountryInput, Vehicle.Class == input$VehicleInput)%>%
    group_by(Name)%>%
    summarize(Clicks=sum(Unique.Visitors))
  ac_Top5_sorted <-
    ac_Top5[order(-ac_Top5$Clicks),]%>%
    top_n(5)
  ac_Top5_sorted
}, height=100, width=200)
output$bar2 <- renderPlot({
  ac_Cat <-
    ac %>%
    select(Country, Vehicle.Class, Category, Unique.Visitors)%>%
    filter(Country == input$CountryInput, Vehicle.Class == input$VehicleInput)
  ac_Cat2 <-
    ac_Cat%>%
    group_by(Country, Category)%>%
    summarize(Visitor_sum2=sum(Unique.Visitors))%>%
    arrange(desc(Visitor_sum2)
            )
  ggplot(ac_Cat2, aes(x=Category, y=Visitor_sum2))+
    geom_col(fill="dodgerblue1")+
    coord_flip()+
    ylab("Clicks")
}, height=400, width=600)
output$gVis <- renderGvis({
  ac_Cat3 <-
    ac%>%
    select(Country, Category, Unique.Visitors)%>%
    filter(Category == input$CategoryInput)
  ac_Cat4 <-
    ac_Cat3%>%
    group_by(Category, Country)%>%
    summarize(Clicks2=sum(Unique.Visitors))
  gvisGeoChart(ac_Cat4, locationvar="Country", 
               colorvar="Clicks2",
               options=list(height=600, width=1000,colorAxis="{colors:['lightgrey', 'green']}"))
})
}

shinyApp(ui=ui, server=server)


