library(dplyr)
library(shinyTree)
#library(shinydashboard)
#library(DT)
library(shinyBS)
#library(dashboardthemes)

country <- read.csv("dat/country")


fluidPage(theme = "style.css",
                  div(style = "padding: 1px 0px; width: '100%'",
                      titlePanel(
                          title = "",
                          windowTitle = "Inventory Calculator"
                      )
                  ),
                  navbarPage(
                      
                      # Application title.
                      title = "InC",
                      # EI Calculator
                      tabPanel(
                          
                          "Calculator",
                          
                          tabsetPanel(
                              
                              type = "tabs",
                              
                              # Circle-packing plot of ethnicity and gender.
                              tabPanel(
                                  
                                  "Activity Input Data",
                                  
                                  # Sidebar panel for controls.
                                  sidebarPanel(
                                      actionButton("clickdiv", "Generate a new Activity Input File",width = "100%"),
                                      fileInput("activityData", h4(" Or "),buttonLabel = "Browse for",
                                                multiple = FALSE,placeholder = "filled Activity Input File",
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"),
                                                )#,
                                      
                          
                                      #tags$p(span("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas laoreet elit ut justo commodo, nec vehicula nulla tincidunt. Aenean congue odio non dolor congue, id pulvinar sapien fringilla. Fusce ac ligula eget dolor bibendum aliquet at eu libero.", style = "color:red"))
                                      ),
                                  
                                  # Main panel with plot.
                                  mainPanel(
                                    fluidRow(
                                      bsModal("modalExample", "Generate Activity Input File", "clickdiv", size = "large",
                                            fluidPage(
                                              # One tab for each plot/table.
                                              tabsetPanel(
                                                
                                                type = "tabs",
                                                
                                                # Circle-packing plot of ethnicity and gender.
                                                tabPanel(
                                                  
                                                  "General Parameters",
                                                  
                                                  # Sidebar panel for controls.
                                                  sidebarPanel(
                                                    selectInput("continent", "Continent",
                                                                unique(country$Continent),selected = "Asia"),
                                                    
                                                    uiOutput("country"),
                                                    sliderInput("range", label = "Inventory years", min = 1990, 
                                                                max = 2020, value = c(2018,2019),step = 1,ticks = T,sep = ""),
                                                    textInput("organisation", label = "Organisation", value = "Enter organization name"),
                                                    textInput("user", label = "Compiled by", value = "Your name"),
                                                    
                                                    tags$p(span("The information you provide above is neccessary to structure the input file and will aid you in keeping records of the calculation as the input file can be reused.", style = "color:red"))
                                                    # tags$p(HTML("<b>Hover</b> to see the part name.")),
                                                    # tags$p(HTML("Each circle represents a <b>unique minifigure or minidoll head</b>.")),
                                                    # tags$p(HTML("Area is proportional to the <b>number of pieces</b> across all sets.")),
                                                    # tags$p(HTML("<b>\"Ethnicity\"</b> is the color of the piece.  Yes, it's silly.")),
                                                    # tags$p(HTML("<b>Gender</b> is inferred from keywords in the part name (\"Male\", \"Female\", etc., plus references to facial hair).")),
                                                    # tags$p("Some heads are not labeled male/female but contain the name of a character of known gender (e.g., \"Han Solo\").  Incorporating this information would require a hand-maintained list of character names and their genders; I haven't done this.")
                                                  ),
                                                  
                                                  # Main panel with plot.
                                                  mainPanel(
                                                    #uiOutput("demographicsCirclePlotUI") %>%
                                                    #withSpinner()
                                                  )
                                                  
                                                ),
                                                
                                                # Bar plot of ethnic diversity and % female.
                                                tabPanel(
                                                  
                                                  "Define Activities",
                                                  
                                                  # Sidebar panel for controls.
                                                  sidebarPanel(
                                                    
                                                    tags$p(HTML("Use the tree to the <b>Right</b> to select the acivities you have data for and want to include in the inventory calculation.")),
                                                    tags$p(HTML("The tree will require a few seconds to load")),
                                                    tags$p(HTML("It is layered as follows:")),
                                                    tags$ul(
                                                      tags$li(HTML("Source sector")),
                                                      tags$li(HTML("Emission factor Tier")),
                                                      tags$li(HTML("Source sub-sector")),
                                                      tags$li(HTML("Sector")),
                                                      tags$li(HTML("Technology")),
                                                      tags$li(HTML("Fuel")),
                                                      tags$li(HTML("Abatement")),
                                                      tags$li(HTML("Region"))
                                                    ),
                                                    tags$p(HTML("Not all activities have a specific <b>Abatement</b> or <b>Region</b> that in this case will feature as <b>NA</b>"))                                  ),
                                                  
                                                  # Main panel with plot.
                                                  mainPanel(
                                                    h3("Activity selection tree:"),
                                                    shinyTree("tree123",checkbox = TRUE)
                                                  )
                                                  
                                                ),
                                                
                                                # Table for finding sets with pieces of particular ethnicity/gender.
                                                tabPanel(
                                                  
                                                  "Download Input File",
                                                  
                                                  # Sidebar panel for controls.
                                                  sidebarPanel(
                                                    downloadButton("downloadData", "Activity Input File")
                                                    
                                                  ),
                                                  
                                                  # Main panel with table.
                                                  mainPanel(
                                                    #dataTableOutput("demographicsSets")
                                                  )
                                                  
                                                )
                                                
                                              )
                                             
                                            )
                                    ),
                                    tableOutput("activityTable")
                                      #uiOutput("demographicsCirclePlotUI") %>%
                                      #withSpinner()
                                    )
                                  )
                                  
                              ),
                              
                              # SUmmary
                              tabPanel(
                                  
                                  "Summaries Across Pollutants",
                                  
                                  # Sidebar panel for controls.
                                  sidebarPanel(
                                      selectInput("sumBy", "Summarize By",
                                                  c("Sector","Type","Technology","Fuel","Abatement","Region","L1","L2")
                                      ),
                                      uiOutput("filterByUI")
                                      
                                      
                                  ),
                                  
                                  # Main panel with plot.
                                  mainPanel(
                                    
                                    tableOutput("emissionAcrossPollutants")   
                                  )
                                  
                              ),
                              
                              # SUmmary 2
                              tabPanel(
                                  
                                  "Summaries By Pollutant",
                                  
                                  # Sidebar panel for controls.
                                  sidebarPanel(
                                      selectInput("sumBy2", "Summarize By",
                                                  c("Sector","Type","Technology","Fuel","Abatement","Region","L1","L2")
                                      ),
                                      uiOutput("filterByUI2")
                                      
                                      
                                  ),
                                  
                                  # Main panel with plot.
                                  mainPanel(
                                      tableOutput("emissionBySpecificPollutant")   
                                  )
                                  
                              ),
                              
                              # Table for finding sets with pieces of particular ethnicity/gender.
                              tabPanel(

                                  "Download Output File",

                                  # Sidebar panel for controls.
                                  sidebarPanel(
                                      downloadButton("downloadOutput", "Computed Emissions")

                                  ),

                                  # Main panel with table.
                                  mainPanel(
                                      #dataTableOutput("demographicsSets")
                                  )

                              )
                              
                          )
                          
                      ),
                      
                      
                     # About and credits.
                      tabPanel(
                          
                          "About",
                          
                          # Various tabs.
                          tabsetPanel(
                              
                              # General info.
                              tabPanel(
                                  "Overview",
                                  uiOutput('pdfView2')
                                  # tags$p(HTML("Parts are labeled and categorized using three main sources of information:")),
                                  # tags$ul(
                                  #     tags$li(HTML("The part category (e.g., \"Minifig Heads\" or \"Plants and Animals\") specified in the database")),
                                  #     tags$li(HTML("The hexadecimal part color specified in the database")),
                                  #     tags$li(HTML("Keywords in the part name"))
                                  # ),
                                  # tags$p(HTML("The keywords that map part names to categories involve more-or-less hand-curated lists and some <i>very</i> basic text processing (mostly regular expressions).  The process is <b>not 100% accurate</b>; there are plenty of false positives and false negatives.  But it's good enough for a first pass.")),
                                  # tags$h1("GitHub"),
                                  # tags$p(HTML("Source code is available at <a href=\"https://github.com/kaplanas/Shiny-Lego\">https://github.com/kaplanas/Shiny-Lego</a>."))
                              ),
                              
                              # Credits.
                              tabPanel(
                                  "Credits",
                                  tags$h2("Datasets and Packages used"),
                                  HTML("<h4>Emission factors and abatement efficiencies are compiled from the 2019 
                                       the <a href=\"https://www.eea.europa.eu/publications/emep-eea-guidebook-2019\">joint EMEP/EEA air pollutant 
                                       emission inventory guidebook</a>. This app has been developped using <a href=\"http://shiny.rstudio.com/\">shiny</a> and the <a href=\"https://www.tidyverse.org/\">tidyverse</a>.</h4>"),
                                  
                                  tags$h2("Developed at the AUB Nature Conservation Center"),
                                  HTML("<h4>The AUB Nature Conservation Center (AUB-NCC) is the only transdisciplinary academic center addressing nature conservation in the MENA region. We leverage the expertise and experience of AUB faculties, 
                                       research staff, and volunteers to tackle the region's most pressing environmental challenges.</h4>"),
                                  
                                  tags$h2("Developed and maintained by"),
                                  HTML("<h4>Anwar Al Shami</h4>"),
                                  HTML("<h4><a href = \"mailto: anwar.g.alshami@gmail.com\">anwar.g.alshami@gmail.com</a></h4>")
                                  
                                  #HTML("<a href=\"http://shiny.rstudio.com/\">shiny</a> and the <a href=\"https://www.tidyverse.org/\">tidyverse</a>, of course.")
                                  # tags$p(html("position and size of the circles in the demographics circle-packing graphs are calculated using <a href=\"https://github.com/thomasp85/ggraph\">ggraph</a>.")),
                                  # tags$p(html("<a href=\"https://igraph.org/r/\">igraph</a> is used to model the hierarchical relationships in the circle-packing graphs, and to construct phylogenetic trees from the graph of wordnet hypernym relationships.")),
                                  # tags$p(html("treemaps, polar charts, and bar charts are rendered with <a href=\"https://www.highcharts.com/\">highcharts</a>, via <a href=\"http://jkunst.com/highcharter/\">highcharter</a>.")),
                                  # tags$p(html("phylogenetic trees are rendered with <a href=\"https://datastorm-open.github.io/visnetwork/\">visnetwork</a>.")),
                                  # tags$p(html("tables are rendered with <a href=\"https://datatables.net/\">datatables</a>, using the <a href=\"https://rstudio.github.io/dt/\">dt</a> package.")),
                                  # tags$p(html("joins with regular expressions are facilitated by <a href=\"https://github.com/dgrtwo/fuzzyjoin\">fuzzyjoin</a>.")),
                                  # tags$p(html("<a href=\"https://github.com/statsmaths/cleanNLP\">cleanNLP</a> and <a href=\"https://cran.r-project.org/web/packages/wordnet/index.html\">wordnet</a> aren't used in the app, but were helpful in exploratory data analysis</a>."))
                              )
                              
                          )
                          
                      )
                      
                  )
          )
