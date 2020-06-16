#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#   

library(shiny)
library(dplyr)
library(shinyTree)
library(shinyjs)
library(data.tree)
source("EI_funs.R")
#data input
country <- read.csv("dat/country")

# EFs <- read.csv("dat/EMEP2019.csv",fileEncoding="UTF-8-BOM")
# AUs <-  read.csv("dat/activityUnitConversions.csv")
#EUs <- read.csv("dat/emissionUnitConversions.csv")

EFs<-readRDS("dat/EFs.rds")
AUs<-readRDS("dat/AUs.rds")
EUs<-readRDS("dat/EUs.rds")
L1names<-read.csv("dat/L1names.csv",stringsAsFactors = F)
L2names<-read.csv("dat/L2names.csv",stringsAsFactors = F)



cleanEID(EFs,AUs,EUs)%>%
    mutate(Type = ifelse(as.character(Type) == "Tier 2 Emission Factor","Tier 2 emission factor", as.character(Type)))%>%
    mutate(Type = ifelse(as.character(Type) == "Tier 1 Emission Factor","Tier 1 emission factor", as.character(Type)))->EFs2
EFs2%>%
    filter(!is.na(L1))%>%
    mutate(L1 = as.integer(L1))%>%
    left_join(L1names)%>%
    mutate(L1 = as.integer(L1))%>%
    left_join(L2names,by = c("L1","L2"))%>%
    filter(grepl("actor", Type))%>%
    arrange(L1,L2)%>%
    distinct(L1,L2,L1name,L2name,Sector,Type,Technology,Abatement,Fuel,activityUnit,Region) %>%
    mutate_each(funs(empty_as_na))%>%
    mutate_if(is.factor, as.character)->distEF
distEF$pathString  <- paste("sel",
                            distEF$L1name,distEF$Type,distEF$L2name,
                            distEF$Sector,
                            distEF$Technology,
                            distEF$Fuel,
                            #distEF$Type,
                            distEF$Abatement,
                            distEF$Region,
                            sep="|")
## load  
acme<-as.Node(distEF,pathDelimiter = "|")

# Define server logic required to draw a histogram
function(input, output, session) {

    output$country <- renderUI({
        selectInput("country", "Country",
                    unique(filter(country,Continent == input$continent)$Country),selected = "Kuwait"
        )
    })
    
    get_json <- reactive({
        treeToJSON(acme, pretty = TRUE)
    })
    
    output$tree123 <- renderTree({get_json()
    })
    ActivityDataInputTable <- reactive({
        tree <- input$tree123
        req(tree)
        sla<-get_selected(tree, format = "slices")
        reshape2::melt(sla)%>%
            filter(complete.cases(L9))%>%
            rename(Region=L9,Fuel=L7,Abatement=L8,Technology=L6,Sector=L5,L2T=L4,Type=L3,L1T=L2,L0=L1)%>%
            rename(L1name=L1T,L2name=L2T)%>%
            select(-L0,-value)%>%
            mutate_each(funs(empty_as_na))%>%
            mutate_if(is.factor, as.character)%>%
            #mutate(L1=as.character(L1))%>%
            #mutate(L2=as.character(L2))%>%
            mutate(Type=as.character(Type))%>%
            mutate(Fuel=as.character(Fuel))%>%
            mutate(Abatement=as.character(Abatement))%>%
            mutate(Technology=as.character(Technology))%>%
            mutate(Sector=as.character(Sector))%>%
            mutate(Region=as.character(Region))%>%
            inner_join(mutate_each(distEF,funs(empty_as_na))%>%
                           mutate_if(is.factor, as.character),
                       by=c("L1name","L2name","Type","Fuel","Abatement","Technology","Sector","Region"))%>%
            filter(complete.cases(activityUnit))
        
        
    })
    output$downloadData <- downloadHandler(
        filename = function() {
            createInputFileName(country = input$country,years = input$range)
        },
        content = function(file) {
            write.csv(tweakInputTable(ActivityDataInputTable(),input$range), file, row.names = FALSE)
        }
    )
    
    output$activityTable <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$activityData)
        activityValues()
    })
    
    activityValues <- reactive({
        req(input$activityData)
        read.csv(input$activityData$datapath)
    })
    
    emissionValues <- reactive({
        req(input$activityData)
        #inputAct<-read.csv("Angola_2018-2019.csv",encoding = "UTF-8-BOM")
        
        computeEmission(activityValues(),EFs2)
        
    })
    
    output$emissionTable <- renderTable(emissionValues())
    
    output$filterByUI <- renderUI({
        req(input$activityData)
        selectInput("filterBy", "Filter By",
                    choices = (unique(emissionValues()[input$sumBy]))
        )
    })
    output$filterByUI2 <- renderUI({
        req(input$activityData)
        selectInput("filterBy2", "Filter By",
                    choices = (unique(emissionValues()["Pollutant"]))
        )
    })
    output$emissionAcrossPollutants<- renderTable({
        req(input$activityData)
        df<-emissionValues()
        summarizeEmissionAcrossPollutants(df,input$sumBy,input$filterBy)
        })
    output$emissionBySpecificPollutant<- renderTable({
        req(input$activityData)
        df<-emissionValues()
        summarizeEmissionBySpecificPollutant(df,input$sumBy2,input$filterBy2)
        })
    output$emissionByPollutants<- renderTable({
        req(input$activityData)
        df<-emissionValues()
        summarizeEmissionByPollutants(df,input$sumBy3)
        })
    output$downloadOutput <- downloadHandler(
        #req(input$activityData),
        filename = function() {
            paste0("emission_",createInputFileName(country = input$country,years = input$range))
        },
        content = function(file) {
            write.csv(emissionValues()%>%
                          select(NFR,Sector,Table,Type,Technology,Fuel,Abatement,Region,Pollutant,Value,Unit,emissionUnit,X2018,X2019,Reference)%>%
                          rename(EFvalue = Value, EFunit = Unit)
            , file, row.names = FALSE)
        }
    )
    output$pdfView2 = renderUI({
        # renderText(input[["referenceChosen"]])#pdfURL = References$URL[as.numeric(input$pdfurl)]
        tags$iframe(style="height:800px; width:100%; scrolling=yes",src =  paste("https://drive.google.com/file/d/1ylO0WBusqVPgmfQm4EZ8_LQoufZFgkQc/preview"))
        
    })
    

}