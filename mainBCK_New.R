## app.R ##
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(dplyr)
library(readr)
library(reshape)
library(rsconnect)
library(readxl)
library(httr)
library(jsonlite)
library(rhandsontable)
#library(hutils)
library(stringi)
library(ggpubr)
library(ggrepel)
library(tidyverse)
library(xlsx)
library(V8)

#rsconnect::setAccountInfo(name='dalysthaitool', token='A62E515AF3CE1B3E1B9E129636F59BA3', secret='9FXZpmvyvpZuq5gtu4RFnQUOsZQbECXHmWhFMaEg')
#General Section----
#..parameters and file location----
options(warn=-1)
DWSheet  <- data.frame()
DurationSheet  <- data.frame()
MasterFile<-"Original/master.xlsx"
SubFile<-"Original/sub.xlsx"
SimFile<-"Original/sim.xlsx"
GroupFile<-"Original/group.xlsx"
#..General functios----
#....Javascript enable function----
jscode <- "shinyjs.collapse = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}"


#....Collapse function----
collapseInput <- function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}
#....Estimate Beta params function----
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
#UI Section----
#..UI Dashboard----
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "PBOD v 4.81",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Version 4.81 Update :",
                                                   message = "Correct YLL 0,0 3,0 3,1",
                                                   icon = icon("life-ring"),
                                                   time = "2021-03-15"),
                                                 messageItem(
                                                   from = "Version 4.8 Update :",
                                                   message = "Correct YLD 0,0 3,0 3,1",
                                                   icon = icon("life-ring"),
                                                   time = "2021-03-13"),
                                                 messageItem(
                                                   from = "Version 4.73 Update :",
                                                   message = "Jitter graph",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-27"),
                                                 messageItem(
                                                   from = "Version 4.72 Update :",
                                                   message = "Saving probabilistic model function",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-27"),
                                                 messageItem(
                                                   from = "Version 4.71 Update :",
                                                   message = "Data probabilistic download function",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-27"),
                                                 messageItem(
                                                   from = "Version 4.71 Update :",
                                                   message = "Plot probabilistic arrange function",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-27"),
                                                 messageItem(
                                                   from = "Version 4.70 Update :",
                                                   message = "remove forecast function",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-26"),
                                                 messageItem(
                                                   from = "Version 4.69 Update :",
                                                   message = "Simulation function beta testing",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-26"),
                                                 messageItem(
                                                   from = "Version 4.68 Update :",
                                                   message = "Alpha, Beta prediction function added",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-22"),
                                                 messageItem(
                                                   from = "Version `` 4.67 Update :",
                                                   message = "Parameter input update",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-21"),
                                                 messageItem(
                                                   from = "Version 4.66 Update :",
                                                   message = "Uncertainty GUIs update.",
                                                   icon = icon("life-ring"),
                                                   time = "2020-09-20"),
                                                 messageItem(
                                                   from = "Version 4.65 Update :",
                                                   message = "Grouping Data and figues download.",
                                                   icon = icon("life-ring"),
                                                   time = "2020-08-16"),
                                                 messageItem(
                                                   from = "Version 4.63 Update :",
                                                   message = "Grouping Data Table",
                                                   icon = icon("life-ring"),
                                                   time = "2020-08-15"),
                                                 messageItem(
                                                   from = "Version 4.62 Update :",
                                                   message = "Grouping Figures Facet",
                                                   icon = icon("life-ring"),
                                                   time = "2020-08-15"),
                                                 messageItem(
                                                   from = "Version 4.61 Update :",
                                                   message = "Add Grouping Data input",
                                                   icon = icon("life-ring"),
                                                   time = "2020-08-13"),
                                                 messageItem(
                                                   from = "Version 4.60 Update :",
                                                   message = "New disease grouping developing",
                                                   icon = icon("life-ring"),
                                                   time = "2020-08-10"),
                                                 messageItem(
                                                   from = "Version 4.52 Update :",
                                                   message = "Short diabetes model uploaded",
                                                   icon = icon("life-ring"),
                                                   time = "2020-06-22"),
                                                 messageItem(
                                                   from = "Version 4.51 Update :",
                                                   message = "Remove number in stack-bar plot",
                                                   icon = icon("life-ring"),
                                                   time = "2020-06-20"),
                                                 messageItem(
                                                   from = "Version 4.50 Update :",
                                                   message = "Multiple mortality rates are accepted.",
                                                   icon = icon("life-ring"),
                                                   time = "2020-06-20")  
                                    )
                    ), 
                    #..UI Sidebar----
                    dashboardSidebar(sidebarMenu
                                     (
                                       id="mainmenu",
                                       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                       menuItem("Current available BOD", tabName = "currentbod", icon = icon("th")),
                                       menuItem("Upload new BOD model Excel", tabName = "newbod", icon = icon("chart-area")),
                                       menuItem("Current available sublevel BOD", tabName = "currentpbod", icon = icon("th")),
                                       menuItem("Add new sublelvel BOD based on existing model", tabName = "newpbod", icon = icon("key")),
                                       menuItem("Create new/view group of diseases/", tabName = "GroupingDisease", icon = icon("object-group")),
                                       menuItem("Special disease models[Soon]", tabName = "GroupingDisease", icon = icon("external-link-square")),
                                       #menuItem("Available Group Models", tabName = "AvailableGroupModel", icon = icon("object-group")),
                                       menuItem("Add Probabilistic Model", tabName = "ProbModel", icon = icon("random")),
                                       menuItem("Available Probabilistic Models", tabName = "AvailableProbModel", icon = icon("random"))#,menuItem("Intervention and forcast", tabName = "invForc", icon = icon("weight")) # Hide forcast tab
                                     )
                    ),
                    #..UI Dashboard body----
                    dashboardBody(
                      useShinyjs(),
                      extendShinyjs(text = jscode, functions = c()),
                      tabItems(
                        #....TAB: DASHBOARD----------------------------------------------
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  infoBox("today", Sys.Date(),
                                          icon = icon("calendar-alt"),color="green"),
                                  infoBoxOutput("countryBox"),
                                  infoBoxOutput("regionBox"),
                                  infoBoxOutput("provinceBox"),
                                  infoBoxOutput("interventionBox")# ,infoBoxOutput("forecastBox"), #hide forcast box
                                )
                        ),
                        #....TAB: CURRENTBOD----------------------------------------------
                        tabItem(tabName = "currentbod",
                                fluidRow(
                                  box(id = "cdiseaseBox", title = "Choose disease:",
                                      status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      selectInput("selDisease", h3("Diseases name:"),""),
                                      selectInput("selcurrentpbodby","Model:",c("[0,0]","[3,0]","[3,1]")),
                                  ),
                                  collapseInput(inputId = "isCollapseDisease", boxId = "cdiseaseBox"),
                                  box(id = "ctransitionBox", title = "Transition parameter:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_transition"),
                                      h6("PS: Incidence and mortality are rate in 100,000 perople.")),
                                  collapseInput(inputId = "isCollapseTransition",
                                                boxId = "ctransitionBox"),
                                  box(id = "cdurationBox", title = "Duration parameter:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_duration"),
                                      h6("NOTE: Leave blank or set zero in non-calculate cells.")),
                                  collapseInput(inputId = "isCollapseDuration", 
                                                boxId = "cdurationBox"),
                                  box(id = "cdwBox", title = "DW parameter:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_dw"),
                                      h6("NOTE: -")),
                                  collapseInput(inputId = "isCollapseDW", boxId = "cdwBox"),
                                  box(id = "cActionBox", title = "Calculate:",
                                      background = "maroon", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      column(12,align="center",
                                             actionButton("cAction", "Calculate and saving files.",
                                                          align="center", icon("paper-plane"), 
                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                  collapseInput(inputId = "isCollapsecAction", 
                                                boxId = "cActionBox"),
                                  box(id = "cPlotBox", title = "Figures:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotOutput(outputId = "DALYPlot"),
                                      downloadLink('downloadFigure','Save figure.')
                                  ),
                                  collapseInput(inputId = "isCollapsecPlot", boxId = "cPlotBox"),
                                  box(id = "cTableBox", title = "Summary Table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_summary"),
                                      downloadLink('downloadData','Download')
                                  ),
                                  collapseInput(inputId = "isCollapsecTable", boxId = "cTableBox"),
                                )
                        ),
                        #....TAB: NEWBOD----------------------------------------------
                        tabItem(tabName = "newbod",
                                fluidRow
                                (
                                  box(id = "uploadnew", title = "Upload new disease:",
                                      status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      textInput("NewDiseaseName", "Disease name:",""),
                                      fileInput("NewDiseaseFile", "Choose .XLSX File",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".xlsx")),
                                      htmlOutput("NewDiseaseResponseText")
                                  )
                                )
                        ),
                        #....TAB: CURRENTPBOD----------------------------------------------
                        
                        tabItem(tabName = "currentpbod",
                                fluidRow(
                                  box(id = "chooseBox", title = "Choose Level:",
                                      status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      selectInput("selcurrentpbodLevel", h3("Level:"),
                                                  c("Region","Province")),
                                      selectInput("selcurrentpbodAvailable", h3("List of current available models:"),
                                                  ""),
                                      
                                      selectInput("selcurrentpbodbysub", h3("Discount models:"),
                                                  c("[0,0]","[3,0]","[3,1]")),
                                  ),
                                  collapseInput(inputId = "isCollapsechooseDisease", boxId = "chooseBox"),
                                  box(id = "choosePlotBox", title = "Figures:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotOutput(outputId = "chooseDALYPlot")),
                                  collapseInput(inputId = "isCollapsechoosePlotBox", boxId = "choosePlotBox"),
                                  box(id = "chooseTableBox", title = "Summary Table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_choosesummary"),
                                  ),
                                  collapseInput(inputId = "isCollapsechooseTable", boxId = "chooseTableBox"),
                                )
                        ),
                        #....TAB: newPBOD-----------------------------------------------
                        tabItem(tabName = "newpbod",
                                fluidRow
                                (
                                  box(id = "uploadnewpbod", title = "Upload new provincial / regional BOD:",
                                      status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      selectInput("selpDisease","Disease:",c("Region","Province")),
                                      selectInput("selpLevel","Level:",c("Region","Province")),
                                      textInput("pLevelName", "Level name:",""),
                                      column(12,align="center",
                                             actionButton("pLoadData", "Load data from existing model",align="center", icon("download"), 
                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                  ),
                                  collapseInput(inputId = "isCollapsepDisease", boxId = "pdiseaseBox"),
                                  box(id = "ptransitionBox", title = "Transition parameter:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_ptransition"),
                                      h6("PS: Incidence and mortality are rate in 10,000 people.")),
                                  collapseInput(inputId = "isCollapsepTransition",
                                                boxId = "ptransitionBox"),
                                  box(id = "pdurationBox", title = "Duration parameter:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_pduration"),
                                      h6("NOTE: Leave blank or set zero in non-calculate cells.")),
                                  collapseInput(inputId = "isCollapsepDuration", 
                                                boxId = "pdurationBox"),
                                  box(id = "pdwBox", title = "DW parameter:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_pdw"),
                                      h6("NOTE: -")),
                                  collapseInput(inputId = "isCollapsepDW", boxId = "pdwBox"),
                                  box(id = "pActionBox", title = "Calculate:",
                                      background = "maroon", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      column(12,align="center",
                                             actionButton("pAction", "Calculate and saving files.",
                                                          align="center", icon("paper-plane"), 
                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                  collapseInput(inputId = "isCollapsepAction", boxId = "pActionBox"),
                                  box(id = "pPlotBox", title = "Figures:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotOutput(outputId = "pDALYPlot")),
                                  collapseInput(inputId = "isCollapsepPlot", boxId = "pplotBox"),
                                  box(id = "pTableBox", title = "Summary Table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_psummary"),
                                  ),
                                  collapseInput(inputId = "isCollapsepTable", boxId = "pTableBox"),
                                )
                        ),
                        #....TAB: InvForc[NOT SHOWN]----------------------------------------------
                        tabItem(tabName = "invForc",
                                fluidRow(
                                  infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                                  box(id = "pfuture", title = "Awaiting function:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      h3("Future function")
                                  )
                                )
                        ),
                        #....TAB: ProbModel-------------------------------
                        tabItem(tabName = "ProbModel",
                                fluidRow(
                                  infoBox("New Probabilistic model:", "", icon = icon("random")),
                                  box(id = "pProb", title = "Create probabilistic function:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      textInput("txtNewProbDisease", "Model name:", 
                                                "New probabilistic model name"),
                                      selectInput("selDiseaseProb","Diseases:",
                                                  c("A","B","C")),
                                      multiInput(
                                        inputId = "idProbParameter", 
                                        label = "Choose parameter to be set.",
                                        choices = c("Banana", "Blueberry"),
                                        selected = "Banana", width = "350px"),
                                      h3("Probabilistic variables:"),
                                      rHandsontableOutput("hot_prob_sel"),
                                      sliderInput("inpMaxRound", "Number of iterations:",
                                                  min = 1, max = 1000, value = 100),
                                      actionButton("cSimulation", "Calculation!!",
                                                   align="center", icon("creative-commons-share"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                                      h3("PLOTS RESULT:"),
                                      h4("Progression:"),
                                      verbatimTextOutput("Progression"),
                                      plotOutput(outputId = "probDALYPlot"),
                                      downloadLink('downloadProbPlot','Download')
                                  ),
                                  box(id = "probTableBox", title = "Summary Disease prob table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_prob_summary"),
                                      downloadLink('downloadProbData','Download')
                                  ),
                                  box(id = "probSavingBox", title = "Saving Model on server options.:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      actionButton("cSavingProb", "Save This Model",align="center", icon("creative-commons-share"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  )
                                )
                        ),
                        #....TAB: AVAILABLE PROB MODEL----
                        tabItem(tabName = "AvailableProbModel",
                                fluidRow(
                                  infoBox("Available Probabilistic model:", "", icon = icon("random")),
                                  box(id = "p_Available_Prob", title = "Available probabilistic function:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      selectInput("selAvailableDiseaseProb","Diseases:",c("A","B","C")),
                                      h3("Probabilistic variables:"),
                                      rHandsontableOutput("hot_available_prob_sel"), 
                                      h4("Number of iterations:"),
                                      verbatimTextOutput("outMaxRound"), 
                                      h3("PLOTS RESULT:"),
                                      plotOutput(outputId = "probAvailableDALYPlot"),
                                      downloadLink('downloadAvailableProbPlot','Download')
                                  ),
                                  box(id = "probAvailableTableBox", title = "Summary Disease prob table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_available_prob_summary"),
                                      downloadLink('downloadAvailableProbData','Download')
                                  )
                                )
                        ),
                        #....TAB: AVAILABLE Group MODEL----
                        tabItem(tabName = "AvailableGroupModel",
                                fluidRow(
                                  infoBox("Available Group model:", "", icon = icon("random")),
                                  box(id = "p_Available_Group", title = "Available Group Models:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      selectInput("selAvailableDiseaseGroup","Diseases:",
                                                  c("A","B","C")),
                                      h3("Group diseases:"),
                                      rHandsontableOutput("hot_available_group_sel"), 
                                      actionButton("cAvailableGroup", "Render Graph!!",
                                                   align="center", icon("creative-commons-share"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                                      h3("PLOTS RESULT:"),
                                      plotOutput(outputId = "groupAvailableDALYPlot"),
                                      downloadLink('downloadAvailableGroupPlot','Download')
                                  ),
                                  box(id = "groupAvailableTableBox", title = "Summary Disease group table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_available_group_summary"),
                                      downloadLink('downloadAvailableGroupData','Download')
                                  )
                                )
                        ),
                        #....TAB: GroupingDisease----
                        tabItem(tabName = "GroupingDisease",
                                fluidRow(
                                  infoBox("Grouping diseases", "create and view disease group", icon = icon("address-book")),
                                  box(id = "pGroup", title = "Grouping Disease with new name.:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      textInput("txtNewDiseaseGroupName", "Disease group", "New Dis."),
                                      selectInput("selNewDiseaseGroupLevel","Level:",c("Country","Region","Province")),
                                      textInput("txtNewDiseaseGroupLevelName", "Level name", ""),
                                      multiInput(
                                        inputId = "idListDisease", label = "Available diseases:",
                                        choices = c("Banana", "Blueberry"),
                                        selected = "Banana", width = "350px"),
                                      actionButton("cCreateGroup", "Create disease group.",
                                                   align="center", icon("creative-commons-share"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                  box(id = "pShowGroup", title = "Show BOD disease group:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      selectInput("selDiseaseGroup", h3("Diseases Group name:"),""),
                                      selectInput("selDiseaseGroupModel","Model:",c("[0,0]","[3,0]","[3,1]")),
                                      actionButton("cRenderGroup", "Rendering group result.",align="center", icon("draw-polygon"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  ),
                                  box(id = "gFigures", title = "Figures in BOD disease group:",
                                      status = "info", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotOutput(outputId = "gDALYPlot"),
                                      downloadLink('downloadGroupFigure','Save figure.')
                                  ),
                                  box(id = "gTableBox", title = "Summary Disease group table:",
                                      status = "success", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      rHandsontableOutput("hot_gsummary"),
                                      downloadLink('downloadGroupData','Download')
                                  )
                                ) #Fluid row
                        ) #tabItem
                      )#End of Tab item
                    )#End of Dashboard body
)#End of Dashboard
#Server Section----
server <- function(session,input, output) {
  #..Essential BOD Function----
  BODCalculation <- function(RootDiseaseName,Level,LevelName,StartYear,StopYear,Intervention,SourceFileName){
    if(Intervention == "Probabilistic"){
      #....BOD Calculation in Probabilistic mode----
      #....Probabilistic mode----
      DWSheet  <- read.xlsx(SourceFileName, sheetName = "DW")
      DurationSheet  <- read.xlsx(SourceFileName, sheetName = "Duration")
      #DWSheet<-DWSheet %>% drop_na()
      DWSheet<-DWSheet %>% replace(is.na(.), 0)
      DurationSheet<-DurationSheet%>% replace(is.na(.), 0)
      #DurationSheet  <-DurationSheet%>% drop_na()
      YLDSheet  <- read.xlsx(SourceFileName, sheetName = "YLD")
      ShortWestTableMale<- read.xlsx(SourceFileName, sheetName = "ShortWestTableMale")
      ShortWestTableFemale<- read.xlsx(SourceFileName, sheetName = "ShortWestTableFemale") 
      TotalColumn<-ncol(YLDSheet)
      DiscountMode<-"[0,0]"
      YLD_TABLE_LIST<-list()
      sim_transheet<-StopYear
      TotalRound<-length(sim_transheet)
      for(k in 1:TotalRound){
        Res<-data.frame()
        withProgress(message = paste0('Please stand by...','round ',k,' from ',TotalRound), min=0, max=TotalRound ,{
          incProgress(k)
          output$Progression<- renderText(paste0("Round ",k," from ",TotalRound," rounds."))
        })
        TransSheet<-sim_transheet[[k]]
        #print(paste0("Round ",k," from ",TotalRound," rounds."))
        #......YLL PREPARE DATA SECTION------
        AveAge<-data.frame("Age"=TransSheet$AgeOfDying)
        #Linear average technique
        Full_West_Mean<-data.frame(Age=0,YLL_0_0=0,YLL_3_1=0,YLL_3_0=0)
        for(iAge in 1:(nrow(AveAge)/2)){
          low_val_male<-ShortWestTableMale[which(ShortWestTableMale$Age==floor(AveAge[iAge,1])),]
          up_val_male<-ShortWestTableMale[which(ShortWestTableMale$Age==ceiling(AveAge[iAge,1])),]
          mean_val_male<-low_val_male + ((AveAge[iAge,1]-floor(AveAge[iAge,1]))*(up_val_male-low_val_male))
          Full_West_Mean[iAge,]<-mean_val_male
        }
        for(iAge in ((nrow(AveAge)/2)+1):nrow(AveAge)){
          low_val_female<-ShortWestTableFemale[which(ShortWestTableFemale$Age==floor(AveAge[iAge,1])),]
          up_val_female<-ShortWestTableFemale[which(ShortWestTableFemale$Age==ceiling(AveAge[iAge,1])),]
          mean_val_female<- low_val_female + ((AveAge[iAge,1]-floor(AveAge[iAge,1]))*(up_val_female-low_val_female))
          Full_West_Mean[iAge,]<-mean_val_female
        }
        YLLwestTable<-Full_West_Mean %>% select(-Age)
        #......Formula interpreter for YLD & YLL----
        for(i in 1:TotalColumn){
          if(!is.na(YLDSheet[1,i]) && YLDSheet[1,i]!='' ){
            ColList<-unlist(strsplit(as.character(YLDSheet[1,i]), "[+*-/]"))
            OperatorList<-unlist(strsplit(as.character(YLDSheet[1,i]), "[0-9]"))
            OperatorList<-stri_remove_empty(OperatorList)
            Ans<-1
            FirstTerm<-0
            for (j in 2:length(ColList)){
              SecondTerm<-as.data.frame(TransSheet[,as.integer(ColList[j])])
              if(FirstTerm==0){
                FirstTerm<-as.data.frame(TransSheet[,as.integer(ColList[j-1])])
              }
              if(OperatorList[j-1]=="*"){
                Ans<-FirstTerm*SecondTerm
              }else if(OperatorList[j-1]=="+"){
                Ans<-FirstTerm+SecondTerm
              }else if(OperatorList[j-1]=="-"){
                Ans<-FirstTerm-SecondTerm
              }else{
                Ans<-FirstTerm/SecondTerm
              }
              FirstTerm<-as.data.frame(Ans)
            }
            if(ncol(Res)==0){
              Res<-cbind(Ans)
            }else{
              Res<-cbind(Res,Ans)
            }
          }else{
            if(ncol(Res)==0){
              Res<-cbind(as.character(YLDSheet[,i]))
            }else{
              Res<-cbind(Res,as.character(YLDSheet[,i]))
            }
          }
        }
        #......YLD CALCULATION----
        YLDPopulation<-Res[1:16,]
        names(YLDPopulation)<-names(DWSheet)
        #print(str(YLDPopulation))
        #print(YLDPopulation)
        DurationSheet<-DurationSheet%>% replace(is.na(.), 0)
        AgeOfOnset<-TransSheet$AgeOfOnset
        #Duration problem dealing
        YLDres_0_0<-YLDPopulation*DWSheet*DurationSheet
        YLDres_3_0<-YLDPopulation*DWSheet*((1-exp(-0.03*DurationSheet))/0.03)
        YLDres_3_1<-YLDPopulation*((0.1658*DWSheet*exp(0.03*AgeOfOnset))/((-0.07)^2))*((exp(-0.07*(DWSheet+AgeOfOnset))*(-0.07*(DWSheet+AgeOfOnset)-1))-(exp(-0.07*(AgeOfOnset))*(-0.07*(AgeOfOnset)-1)))
        UpperBorder<-c(5,15,30,45,60,70,80)
        for(i in c(1:8)){
          UpperBorder[i]
        }
        #......YLD TABLE PREPARATION----
        YLDres_0_0<-YLDres_0_0 %>% drop_empty_cols() %>% mutate(YLD_0_0=rowSums(.)) %>% select(YLD_0_0)
        YLDres_3_0<-YLDres_3_0 %>% drop_empty_cols() %>% mutate(YLD_3_0=rowSums(.))%>% select(YLD_3_0)
        YLDres_3_1<-YLDres_3_1 %>% drop_empty_cols() %>% mutate(YLD_3_1=rowSums(.))%>% select(YLD_3_1)
        FinalTable<-data.frame("Age"=as.factor(TransSheet$Age),"Sex"=c(rep("Male",8),rep("Female",8)))
        FinalTable<-cbind(FinalTable,YLDres_0_0,YLDres_3_0,YLDres_3_1)
        #......YLL CALCULATION SECTION----
        t<-TransSheet%>% select(starts_with("Mortality"))
        df_c<-names(t)
        PerNumMort<-as.integer(strsplit(df_c,"Per")[[1]][2])
        t<-t/PerNumMort
        YLLNum<-TransSheet$Population*t
        tYLL_0_0<-YLLNum*YLLwestTable$YLL_0_0
        tYLL_3_1<-YLLNum*YLLwestTable$YLL_3_1
        tYLL_3_0<-YLLNum*YLLwestTable$YLL_3_0
        t<-data.frame(YLL_0_0=rowSums(tYLL_0_0),YLL_3_0=rowSums(tYLL_3_0),YLL_3_1=rowSums(tYLL_3_1))
        FinalTable<-cbind(FinalTable,t)
        h<-names(TransSheet)
        for(jIncidence in 1:length(h)){
          if(tolower(substr(h[jIncidence],1,9))=="incidence"){
            df_c<-names(h[jIncidence])
            PerNumIncidence<-as.integer(strsplit(df_c,"Per")[[1]][2])
            #t<-t/PerNumIncidence
            TransSheet[h[jIncidence]]=TransSheet[h[jIncidence]]*PerNumIncidence
          }
        }
        #TransSheet$Incidence<-TransSheet$Incidence*100000
        #TransSheet$Mortality<-TransSheet$Mortality*100000
        #......YLL TABLE PREPARATION----
        FinalTable<-FinalTable %>% mutate(DALY_0_0=YLD_0_0+YLL_0_0,DALY_3_0=YLD_3_0+YLL_3_0,DALY_3_1=YLD_3_1+YLL_3_1)
        FinalTable$Age<-FinalTable$Age %>% factor(levels=c("0-4","5-14","15-29","30-44","45-59","60-69","70-79","80+"))
        #print(TargetFile)
        #print(FinalTable)
        #print("FINAL TABLE=")
        #......LIST OF ALL TABLES----
        YLD_TABLE_LIST[[k]]<-FinalTable
      }
      return(YLD_TABLE_LIST)
    }else{
      #....Stochastic----
      print("Level=")
      print(Level)
      print("End of level")
      tmpdis<-input$selDisease
      vdis<-strsplit(tmpdis," :")
      if(Level=="Region"){
        TransSheet<- hot_to_r(req(input$hot_ptransition))
        DWSheet<- hot_to_r(req(input$hot_pdw))
        DurationSheet<- hot_to_r(req(input$hot_pduration))
        DiscountMode<-input$selcurrentpbodbysub
      }else if (Level=="Province"){
        TransSheet<- hot_to_r(req(input$hot_ptransition))
        DWSheet<- hot_to_r(req(input$hot_pdw))
        DurationSheet<- hot_to_r(req(input$hot_pduration))
        DiscountMode<-input$selcurrentpbodbysub
      }else if (Level=="ShowSubLevel"){
        TransSheet<- read.xlsx(SourceFileName, sheetName = "Trans")
        DWSheet<- read.xlsx(SourceFileName, sheetName = "DW")
        DurationSheet<- read.xlsx(SourceFileName, sheetName = "Duration")
        DiscountMode<-input$selcurrentpbodbysub
      }else{
        
        
        FlName<-paste0("Original/",unlist(vdis)[1],"xlsx")
        DWSheet  <- read.xlsx(FlName, sheetName = "DW")#read YLD sheet
        DurationSheet  <- read.xlsx(FlName, sheetName = "Duration")  # read YLD sheet
        DWSheet<-DWSheet %>% drop_na()
        DurationSheet  <-DurationSheet%>% drop_na()
        ViewDurationSheet<- hot_to_r(req(input$hot_duration))
        for (i in 1:ncol(ViewDurationSheet)){
          colName<-names(ViewDurationSheet)[i]
          t<-ViewDurationSheet[,i]
          DurationSheet[,colName]=t
        }
        ViewDWSheet<- hot_to_r(req(input$hot_dw))
        for (i in 1:ncol(ViewDWSheet)){
          colName<-names(ViewDWSheet)[i]
          t<-ViewDWSheet[,i]
          DWSheet[,colName]=t
        }
        TransSheet<- hot_to_r(req(input$hot_transition))
        DiscountMode<-input$selcurrentpbodby
      }
      
      
      
      #......YLD DATA PREPARATION----
      #print("Soure file name=")
      #print(SourceFileName)
      YLDSheet  <- read.xlsx(SourceFileName, sheetName = "YLD")
      ShortWestTableMale<- read.xlsx(SourceFileName, sheetName = "ShortWestTableMale")
      ShortWestTableFemale<- read.xlsx(SourceFileName, sheetName = "ShortWestTableFemale")
      TotalColumn<-ncol(YLDSheet)
      Res<-data.frame()
      
      df_c<-names(TransSheet)
      inci<-df_c[startsWith(df_c,"Mort")]
      for (cName in inci){
        strsplit(cName,"Per")
        PerNum<-strsplit(cName,"Per")[[1]][2]
        PerNum<-as.integer(PerNum)
        TransSheet[[cName]]<-TransSheet[[cName]]/PerNum
      }
      
      df_c<-names(TransSheet)
      inci<-df_c[startsWith(df_c,"Incidence")]
      for (cName in inci){
        strsplit(cName,"Per")
        PerNum<-strsplit(cName,"Per")[[1]][2]
        PerNum<-as.integer(PerNum)
        TransSheet[[cName]]<-TransSheet[[cName]]/PerNum
      }
      
      
      #t<-t/PerNumIncidence
      #TransSheet[h[j]]=TransSheet[h[j]]*PerNumIncidence
      #......FORMULA INTERPRETER----
      for(i in 1:TotalColumn){
        if(!is.na(YLDSheet[1,i]) && YLDSheet[1,i]!='' ){
          #print(paste(" sub i =",i))
          ColList<-unlist(strsplit(as.character(YLDSheet[1,i]), "[+*-/]"))
          OperatorList<-unlist(strsplit(as.character(YLDSheet[1,i]), "[0-9]"))
          OperatorList<-stri_remove_empty(OperatorList)
          Ans<-1
          FirstTerm<-0
          for (j in 2:length(ColList)){
            SecondTerm<-as.data.frame(TransSheet[,as.integer(ColList[j])])
            if(FirstTerm==0){
              FirstTerm<-as.data.frame(TransSheet[,as.integer(ColList[j-1])])
            }
            if(OperatorList[j-1]=="*"){
              Ans<-FirstTerm*SecondTerm
            }else if(OperatorList[j-1]=="+"){
              Ans<-FirstTerm+SecondTerm
            }else if(OperatorList[j-1]=="-"){
              Ans<-FirstTerm-SecondTerm
            }else{
              Ans<-FirstTerm/SecondTerm
            }
            FirstTerm<-as.data.frame(Ans)
          }
          if(ncol(Res)==0){
            Res<-cbind(Ans)
          }else{
            Res<-cbind(Res,Ans)
          }
        }else{
          if(ncol(Res)==0){
            Res<-cbind(as.character(YLDSheet[,i]))
          }else{
            Res<-cbind(Res,as.character(YLDSheet[,i]))
          }
        }
      }
      #......YLD CALCULATION----        
      YLDPopulation<-Res[1:16,]
      
      names(YLDPopulation)<-names(DWSheet)
      
      DWSheet<-DWSheet %>% replace(is.na(.), 0)
      DurationSheet<-DurationSheet%>% replace(is.na(.), 0)
      AgeOfOnset<-TransSheet$AgeOfOnset
      #Duration problem dealing
      YLDres_0_0<-YLDPopulation*DWSheet*DurationSheet
      #View(YLDPopulation)
      #View(DWSheet)
      #View(DurationSheet)
      
      YLDres_3_0<-YLDPopulation*DWSheet*((1-exp(-0.03*DurationSheet))/0.03)
      YLDres_3_1<-YLDPopulation*((0.1658*DWSheet*exp(0.03*AgeOfOnset))/((-0.07)^2))*((exp(-0.07*(DurationSheet+AgeOfOnset))*(-0.07*(DurationSheet+AgeOfOnset)-1))-(exp(-0.07*(AgeOfOnset))*(-0.07*(AgeOfOnset)-1)))
      
      #YLDres_3_1<-YLDPopulation*((0.1658*DWSheet*exp(0.03*AgeOfOnset))/((-0.07)^2))*((exp(-0.07*(DurationSheet+AgeOfOnset))*(-0.07*(DurationSheet+AgeOfOnset)-1))-(exp(-0.07*(AgeOfOnset))*(-0.07*(AgeOfOnset)-1)))
      #View(YLDres_3_1)
      UpperBorder<-c(5,15,30,45,60,70,80)
      for(i in c(1:8)){
        UpperBorder[i]
      }
      #......YLD TABLE PREPARATION---- 
      YLDres_0_0<-YLDres_0_0 %>% drop_empty_cols() %>% mutate(YLD_0_0=rowSums(.)) %>% select(YLD_0_0)
      YLDres_3_0<-YLDres_3_0 %>% drop_empty_cols() %>% mutate(YLD_3_0=rowSums(.))%>% select(YLD_3_0)
      YLDres_3_1<-YLDres_3_1 %>% drop_empty_cols() %>% mutate(YLD_3_1=rowSums(.))%>% select(YLD_3_1)
      FinalTable<-data.frame("Age"=as.factor(TransSheet$Age),"Sex"=c(rep("Male",8),rep("Female",8)))
      FinalTable<-cbind(FinalTable,YLDres_0_0,YLDres_3_0,YLDres_3_1)
      #......YLL DATA PREPARATION---- 
      AveAge<-data.frame("Age"=TransSheet$AgeOfDying)
      #Linear average
      Full_West_Mean<-data.frame(Age=0,YLL_0_0=0,YLL_3_1=0,YLL_3_0=0)
      for(iAge in 1:(nrow(AveAge)/2)){
        low_val_male<-ShortWestTableMale[which(ShortWestTableMale$Age==floor(AveAge[iAge,1])),]
        up_val_male<-ShortWestTableMale[which(ShortWestTableMale$Age==ceiling(AveAge[iAge,1])),]
        mean_val_male<-low_val_male + ((AveAge[iAge,1]-floor(AveAge[iAge,1]))*(up_val_male-low_val_male))
        Full_West_Mean[iAge,]<-mean_val_male
      }
      for(iAge in ((nrow(AveAge)/2)+1):nrow(AveAge)){
        low_val_female<-ShortWestTableFemale[which(ShortWestTableFemale$Age==floor(AveAge[iAge,1])),]
        up_val_female<-ShortWestTableFemale[which(ShortWestTableFemale$Age==ceiling(AveAge[iAge,1])),]
        mean_val_female<- low_val_female + ((AveAge[iAge,1]-floor(AveAge[iAge,1]))*(up_val_female-low_val_female))
        Full_West_Mean[iAge,]<-mean_val_female
      }
      YLLwestTable<-Full_West_Mean %>% select(-Age)
      print("This is YLL West Table")
      print(YLLwestTable)
      #......YLL CALCULATION---- 
      t<-TransSheet%>% select(starts_with("Mortality"))
      YLLNum<-TransSheet$Population*t
      tYLL_0_0<-YLLNum*YLLwestTable$YLL_0_0
      tYLL_3_1<-YLLNum*YLLwestTable$YLL_3_1
      tYLL_3_0<-YLLNum*YLLwestTable$YLL_3_0
      t<-data.frame(YLL_0_0=rowSums(tYLL_0_0),YLL_3_0=rowSums(tYLL_3_0),YLL_3_1=rowSums(tYLL_3_1))
      FinalTable<-cbind(FinalTable,t)
      h<-names(TransSheet)
      for(j in 1:length(h)){
        if(tolower(substr(h[j],1,9))=="incidence"){
          df_c<-h[j]
          PerNumIncidence<-as.integer(strsplit(df_c,"Per")[[1]][2])
          TransSheet[h[j]]=TransSheet[h[j]]*PerNumIncidence
        }
        if(tolower(substr(h[j],1,9))=="mortality"){
          df_c<-h[j]
          PerNumMort<-as.integer(strsplit(df_c,"Per")[[1]][2])
          TransSheet[h[j]]=TransSheet[h[j]]*PerNumMort
        }
      }
      #......DALY TABLE PREPARATION---- 
      FinalTable<-FinalTable %>% mutate(DALY_0_0=YLD_0_0+YLL_0_0,DALY_3_0=YLD_3_0+YLL_3_0,DALY_3_1=YLD_3_1+YLL_3_1)
      FinalTable$Age<-FinalTable$Age %>% factor(levels=c("0-4","5-14","15-29","30-44","45-59","60-69","70-79","80+"))
      
      TargetFile<-paste0("result/",unlist(vdis)[1],"xlsx")
      print(TargetFile)
      print(FinalTable)
      #......SAVING RESULT FILE---- 
      if(Level=="Country"){
        
        TargetFile<-paste0("result/Country/",unlist(vdis)[1],"xlsx")
      }else if(Level=="Region"){
        print("THIS IS NEW Region BOD.!!")
        tmpdis<-input$selpDisease
        vdis<-strsplit(tmpdis," :")
        TargetFile<-paste0("result/Region/",unlist(vdis)[1],"xlsx")
      }else if(Level=="Province"){
        print("THIS IS NEW Province BOD.!!")
        tmpdis<-input$selpDisease
        vdis<-strsplit(tmpdis," :")
        TargetFile<-paste0("result/Province/",unlist(vdis)[1],"xlsx")
      }else{
        stop("The value is TRUE, so the script must end here")
      }
      print(TargetFile)
      print(FinalTable)
      write.xlsx(FinalTable, file=TargetFile, sheetName="Main", row.names=FALSE)
      
      
      #write.xlsx(FinalTable, file=TargetFile, sheetName="Main", row.names=FALSE)
      #......PLOT WITH specific discount mode---- 
      ###Filter only Type Need
      if(DiscountMode=="[0,0]"){
        plot_YLD<- FinalTable %>% ggplot(aes(x=Age,y=YLD_0_0,fill =Sex)) +
          geom_col() + ggtitle("Fig 1: YLD plot [0,0]")+ theme_minimal()+ 
          rremove("xlab")+ rremove("x.text")+ rremove("x.grid")
        plot_YLL<- FinalTable %>% ggplot(aes(x=Age,y=YLL_0_0,fill =Sex)) +
          geom_col() + ggtitle("Fig 2: YLL plot [0,0]")+ theme_minimal()+ 
          rremove("xlab")+ rremove("x.text")+ rremove("x.grid")
        plot_DALY <- FinalTable %>% ggplot(aes(x=Age,y=DALY_0_0,fill =Sex)) +
          geom_col() + ggtitle("Fig 3: DALY plot [0,0]")+ theme_minimal()+ rremove("x.grid")
        FinalTable<-FinalTable %>% select("Age","Sex","YLD_0_0","YLL_0_0","DALY_0_0")
      }else if(DiscountMode=="[3,0]"){
        plot_YLD<- FinalTable %>% ggplot(aes(x=Age,y=YLD_3_0,fill =Sex)) +geom_col()
        plot_YLL<- FinalTable %>% ggplot(aes(x=Age,y=YLL_3_0,fill =Sex)) +geom_col()
        plot_DALY <- FinalTable %>% ggplot(aes(x=Age,y=DALY_3_0,fill =Sex)) +geom_col()
        FinalTable<-FinalTable %>% select("Age","Sex","YLD_3_0","YLL_3_0","DALY_3_0")
      }else{
        plot_YLD<- FinalTable %>% ggplot(aes(x=Age,y=YLD_3_1,fill =Sex)) +geom_col()
        plot_YLL<- FinalTable %>% ggplot(aes(x=Age,y=YLL_3_1,fill =Sex)) +geom_col()
        plot_DALY <- FinalTable %>% ggplot(aes(x=Age,y=DALY_3_1,fill =Sex)) +geom_col()
        FinalTable<-FinalTable %>% select("Age","Sex","YLD_3_1","YLL_3_1","DALY_3_1")
      }
      gridExtra::grid.arrange(plot_YLD, plot_YLL, plot_DALY, ncol = 1)
      plot_all<-ggarrange(plot_YLD, plot_YLL, plot_DALY, labels = c("A.", "B.", "C."),ncol = 1, nrow = 3)  
      ggsave("plot.pdf", plot_all)
      #......RETURN PLOT AND TABLE---- 
      ret<-list(FinalTable,plot_all)  
      #......Saving file section----
      if(Level=="ShowSubLevel"){
        #shinyjs::alert("Come here")
      }else{
        if(Level=="Country"){
          FlName<-SourceFileName
        }else{
          #......SAVING SUB LEVEL DATA----         
          SubSheet<-read.xlsx(SubFile, sheetName = "Sub")
          nextID<-max(as.integer(SubSheet$ID))+1
          NewFlName<-paste0("Sub/",nextID,".xlsx",sep="")
          #ID	Disease	Level	Name	StartYear	StopYear	Intervention	Source	File
          SubSheet$ID<-as.integer(SubSheet$ID)
          SubSheet$Disease<-as.character(SubSheet$Disease)
          SubSheet$Level<-as.character(SubSheet$Level)
          SubSheet$Name<-as.character(SubSheet$Name)
          SubSheet$StartYear<-as.character(SubSheet$StartYear)
          SubSheet$StopYear<-as.character(SubSheet$StopYear)
          SubSheet$Intervention<-as.character(SubSheet$Intervention)
          SubSheet$Source<-as.character(SubSheet$Source)
          SubSheet$File<-as.character(SubSheet$File)
          NewRow<-c(nextID,RootDiseaseName,Level,LevelName,
                    StartYear,StopYear,Intervention,SourceFileName,NewFlName)
          SubSheet<-rbind(SubSheet,NewRow)
          SubSheet <- SubSheet%>% replace(., is.na(.), "")
          write.xlsx(SubSheet,file="Original/sub.xlsx",
                     sheetName = "Sub",row.names = FALSE,append = F)
          FlName<-NewFlName
        }
        YLDSheet <- YLDSheet%>% replace(., is.na(.), "")
        if(TRUE){
          #for(j in 1:length(h)){
          #  if(tolower(substr(h[j],1,9))=="incidence"){
          #    TransSheet[h[j]]=TransSheet[h[j]]/10000
          #  }
          #}
          # View(TransSheet)
          # df_c<-names(TransSheet)
          # inci<-df_c[startsWith(df_c,"Mort")]
          # for (cName in inci){
          #   strsplit(cName,"Per")
          #   PerNum<-strsplit(cName,"Per")[[1]][2]
          #   PerNum<-as.integer(PerNum)
          #   TransSheet[[cName]]<-TransSheet[[cName]]*PerNum
          # }
          # 
          # df_c<-names(TransSheet)
          # inci<-df_c[startsWith(df_c,"Incidence")]
          # for (cName in inci){
          #   strsplit(cName,"Per")
          #   PerNum<-strsplit(cName,"Per")[[1]][2]
          #   PerNum<-as.integer(PerNum)
          #   TransSheet[[cName]]<-TransSheet[[cName]]*PerNum
          # }
          
          write.xlsx(TransSheet, file=FlName, sheetName="Trans", row.names=FALSE)
          write.xlsx(YLDSheet, file=FlName, sheetName="YLD", append=TRUE, row.names=FALSE)
          write.xlsx(DWSheet, file=FlName, sheetName="DW", append=TRUE, row.names=FALSE)
          write.xlsx(DurationSheet, file=FlName, sheetName="Duration", append=TRUE, row.names=FALSE)
          write.xlsx(ShortWestTableMale, file=FlName, 
                     sheetName="ShortWestTableMale", append=TRUE, row.names=FALSE)
          write.xlsx(ShortWestTableFemale, file=FlName, 
                     sheetName="ShortWestTableFemale", append=TRUE, row.names=FALSE)
          shinyjs::alert("Saving file done!")
        }
      }
      return(ret)
    }
  }
  
  
  
  #Initialize in all pages----------------------------------------------
  #Closing box in initial phase
  ##js$collapse("cdiseaseBox")
  
  # #js$collapse("cdurationBox")
  # #js$collapse("ctransitionBox")
  # #js$collapse("cdwBox")
  # #js$collapse("cActionBox")
  # #js$collapse("cPlotBox")
  # #js$collapse("cTableBox")
  # #js$collapse("choosePlotBox")
  # #js$collapse("chooseTableBox")
  # 
  #..OBSERVE EVENT SECTION----
  #....OBSERVE EVENT PAGE SELECTION----
  observeEvent(req(input$mainmenu), {
    #shinyjs::alert(input$mainmenu)
    
    if((input$mainmenu=="currentbod")|| (input$mainmenu=="newpbod")){
      #......PAGE currentbod or newpbod SELECTED----
      #ID	Disease	File
      MasterSheet<- read.xlsx(MasterFile, sheetName = "Master")
      MasterSheet <- MasterSheet %>% mutate(Detail = paste0(ID,'. :',Disease))
      MasterSheet <- rbind(c(0,"","","Select disease"),MasterSheet)
      updateSelectInput(session, "selDisease", 
                        label = "Choose disease to show its detail.", 
                        choices = MasterSheet$Detail,
                        selected = NULL)
      updateSelectInput(session, "selpDisease", 
                        label = "Select base disease's model:.", 
                        choices = MasterSheet$Detail,
                        selected = NULL)
      ##js$collapse("cdiseaseBox")
    }else if(input$mainmenu=="currentpbod"){
      #......PAGE currentpbod or newpbod SELECTED----
      #List all 
      ##js$collapse("choosePlotBox")
      ##js$collapse("chooseTableBox")
    }else if(input$mainmenu=="GroupingDisease"){
      #......PAGE GroupingDisease SELECTED----
      MasterSheet<- read.xlsx(MasterFile, sheetName = "Master")
      MasterSheet <- MasterSheet %>% mutate(Detail = paste0(ID,'. :',Disease))
      updateSelectInput(session, "idListDisease", 
                        choices = MasterSheet$Detail,
                        selected = NULL)
      GroupSheet<- read.xlsx(GroupFile, sheetName = "Main")
      ListName<-GroupSheet %>% mutate(LineItem=paste0(ID,". ",GroupName," level:",Level," ",LevelName))
      ListName$LineItem<-gsub("NA","",ListName$LineItem)
      updateSelectInput(session, "selDiseaseGroup", 
                        choices = ListName$LineItem,
                        selected = NULL)
    }else if(input$mainmenu=="AvailableProbModel"){
      #......PAGE AvailableProbModel selected----
      SimSheet<-read.xlsx(SimFile,sheetName = "Sim")
      SimSheet<-SimSheet %>% mutate(Detail = paste0(ID,'. :',Name,", Base ",BaseDisease))
      ExpList<-SimSheet 
      ShowList<-ExpList$Detail
      ShowList<-rbind("Please select from below",ShowList)
      updateSelectInput(session, "selAvailableDiseaseProb", 
                        label = "Choose available subregion model.", 
                        choices = ShowList,
                        selected = NULL)
    }else if(input$mainmenu=="ProbModel"){
      #......PAGE ADD PROB MODEL PAGE SELECTED----
      MasterSheet<- read.xlsx(MasterFile, sheetName = "Master")
      MasterSheet <- MasterSheet %>% mutate(Detail = paste0(ID,'. :',Disease))
      updateSelectInput(session, "selDiseaseProb", 
                        choices = MasterSheet$Detail,
                        selected = NULL)
      SubSheet<-read.xlsx(SubFile,sheetName = "Sub")
      ListName<-SubSheet %>% mutate(LineItem=paste0(ID,". ",Disease," level:",Level," ",Name))
      ListName$LineItem<-gsub("NA","",ListName$LineItem)
      updateSelectInput(session, "selDiseaseProb", 
                        choices = ListName$LineItem,
                        selected = NULL)
      updateSelectInput(session, "idProbParameter", 
                        choices = MasterSheet$Detail,
                        selected = NULL)
    }else if(input$mainmenu=="dashboard"){
      #......PAGE MAIN DASHBOARD SELECTED----
      MasterSheet<-read.xlsx(MasterFile,sheetName = "Master")
      numCountry<-max(as.numeric(MasterSheet$ID))
      SubSheet<-read.xlsx(SubFile,sheetName = "Sub")
      numRegion<-nrow(subset(SubSheet, Level=="Region"))
      numProvince<-nrow(subset(SubSheet, Level=="Province"))
      numInv<-nrow(subset(SubSheet, Intervention!=""))
      numForc<-nrow(subset(SubSheet, StopYear!=""))
      #........UPDATE INFO BOX ----
      output$countryBox <- renderInfoBox({
        infoBox(
          "Country models:", paste0(numCountry, ""), icon = icon("globe"),
          color = "blue")
      })
      output$regionBox <- renderInfoBox({
        infoBox(
          "Regional models:", paste0(numRegion , ""), icon = icon("map"),
          color = "purple")
      })
      output$provinceBox <- renderInfoBox({
        infoBox(
          "Provincial models:", paste0(numProvince , ""), icon = icon("map-marker-alt"),
          color = "orange")
      })
      output$interventionBox <- renderInfoBox({
        infoBox(
          "Probabilistic models:", paste0(numInv , ""), icon = icon("balance-scale"),
          color = "maroon")
      })
      output$forecastBox <- renderInfoBox({
        infoBox(
          "Forecast models:", paste0("-",numForc , "-"), icon = icon("glasses"),
          color = "fuchsia")
      })
    }
  })
  #....OBSERVE EVENT FOR PBOD PAGE----
  observeEvent(req(input$selcurrentpbodLevel),{
    SubSheet<-read.xlsx(SubFile,sheetName = "Sub")
    #ID	Disease	Level	Name	StartYear	StopYear	Intervention	Source	File
    SubSheet<-SubSheet %>% mutate(Detail = paste0(ID,'. :',Disease," ,",Level,": ",Name))
    ExpList<-SubSheet %>% filter(Level==input$selcurrentpbodLevel)
    ShowList<-ExpList$Detail
    ShowList<-rbind("Please select from below",ShowList)
    updateSelectInput(session, "selcurrentpbodAvailable", 
                      label = "Choose available subregion model.", 
                      choices = ShowList,
                      selected = NULL)
  })
  #....OBSERVE EVENT FOR CURRENT PBOD AVAILABLE PAGE----
  observeEvent({input$selcurrentpbodAvailable
    input$selcurrentpbodby},{
      if(input$mainmenu=="currentpbod"){
        if((input$selcurrentpbodAvailable!='') && (input$selcurrentpbodAvailable!="Please select from below")){
          if(!is.null(input$isCollapsechoosePlotBox) && input$isCollapsechoosePlotBox==TRUE)
          {
            #js$collapse("choosePlotBox")
            #js$collapse("chooseTableBox")
          }
          vdis<-strsplit(input$selcurrentpbodAvailable,". :")
          RootDiseaseName<-as.character(unlist(vdis)[2])
          SourceFileName<-paste0("Sub/",unlist(vdis)[1],".xlsx")
          res<-BODCalculation(RootDiseaseName,"ShowSubLevel","","","","",SourceFileName)
          output$chooseDALYPlot<-renderPlot({
            res[2]
          })
          output$hot_choosesummary <- renderRHandsontable({
            rhandsontable(as.data.frame(res[1]),readOnly = TRUE) %>% 
              hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
          })
        }
      }
    })
  #....OBSERVE EVENT FOR CURRENT BOD PAGE----
  observeEvent(req(input$selDisease), {
    #......Collapse all boxes----
    if(input$selDisease !="Select disease" && !is.null(input$selDisease)){
      if(!is.null(input$isCollapseTransition) && input$isCollapseTransition){
        #js$collapse("ctransitionBox")
      }
      if(!is.null(input$isCollapseDuration) && input$isCollapseDuration){
        #js$collapse("cdurationBox")
      }
      if(!is.null(input$isCollapseDW) && input$isCollapseDW){
        #js$collapse("cdwBox")
      }
      if(!is.null(input$isCollapsecAction) && input$isCollapsecAction){
        #js$collapse("cActionBox")
      }
      if(!is.null(input$isCollapsecTable) && !(input$isCollapsecTable)){
        #js$collapse("cTableBox")
        output$hot_summary <- renderRHandsontable({})
      }
      if(!is.null(input$isCollapsecPlot) && !(input$isCollapsecPlot)){
        #js$collapse("cPlotBox")
        output$DALYPlot<-renderPlot({})
      }
      #......Show relavant disease parameter----
      tmpdis<-input$selDisease
      vdis<-strsplit(tmpdis," :")
      FlName<-paste0("Original/",unlist(vdis)[1],"xlsx")
      TransSheet  <- read.xlsx(FlName, sheetName = "Trans") 
      YLDSheet  <- read.xlsx(FlName, sheetName = "YLD")
      DWSheet  <- read.xlsx(FlName, sheetName = "DW")
      DurationSheet  <- read.xlsx(FlName, sheetName = "Duration")
      #TransSheet <- TransSheet %>% drop_na()
      DWheet <- DWSheet %>% drop_na()
      DurationSheet <- DurationSheet %>% drop_na()
      # read YLD sheet
      ShortWestTableMale<- read.xlsx(FlName, sheetName = "ShortWestTableMale")  
      ShortWestTableFemale<- read.xlsx(FlName, sheetName = "ShortWestTableFemale")  
      #......Render HOT Table----
      output$hot_transition <- renderRHandsontable({
        rhandsontable(TransSheet,readOnly = FALSE) %>% 
          hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      })
      output$hot_dw <- renderRHandsontable({
        ViewDWSheet<-DWSheet
        ViewDWSheet<-ViewDWSheet[, colSums(ViewDWSheet != 0) > 0]
        rhandsontable(ViewDWSheet,readOnly = FALSE) %>% 
          hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      })
      output$hot_duration <- renderRHandsontable({
        ViewDurationSheet<-DurationSheet
        #print(ViewDurationSheet)
        ViewDurationSheet<-ViewDurationSheet[, colSums(ViewDurationSheet != 0) > 0]
        rhandsontable(ViewDurationSheet,readOnly = FALSE) %>% 
          hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      })
    }else{
      #......Collapse/Uncollapse all boxes----
      if(!is.null(input$isCollapseTransition) && !(input$isCollapseTransition)){
        #js$collapse("ctransitionBox")
        output$hot_transition <- renderRHandsontable({})
      }
      if(!is.null(input$isCollapseDuration) && !(input$isCollapseDuration)){
        #js$collapse("cdurationBox")
        output$hot_duration <- renderRHandsontable({})
      }
      if(!is.null(input$isCollapseDW) && !(input$isCollapseDW)){
        #js$collapse("cdwBox")
        output$hot_dw <- renderRHandsontable({})
      }
      if(!is.null(input$isCollapsecAction) && !(input$isCollapsecAction)){
        #js$collapse("cActionBox")
      }
      if(!is.null(input$isCollapsecTable) && !(input$isCollapsecTable)){
        #js$collapse("cTableBox")
        output$hot_summary <- renderRHandsontable({})
      }
      if(!is.null(input$isCollapsecPlot) && !(input$isCollapsecPlot)){
        #js$collapse("cPlotBox")
        output$DALYPlot<-renderPlot({})
      }
    }
  }) 
  #Calculation section
  #....OBSERVE EVENT FOR DOWNLOAD LINK----
  
  #......DOWNLOAD BOD PLOT----
  output$downloadFigure<-downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      file.copy("plot.pdf", file, overwrite=TRUE)
    }
  )
  #......DOWNLOAD BOD DATA----
  output$downloadData<-downloadHandler(filename = function(){
    paste('data-',Sys.Date(),'.csv',sep='')},
    content = function(con){
      inputtmp<- hot_to_r(req(input$hot_summary))
      write.csv(inputtmp,con)
    }
  )
  #......DOWNLOAD GROUP BOD PLOT----
  output$downloadGroupFigure<-downloadHandler(
    filename = function() {
      "plotGroup.pdf"
    },
    content = function(file) {
      file.copy("plotGroup.pdf", file, overwrite=TRUE)
    }
  )
  #......DOWNLOAD GROUP BOD DATA----
  output$downloadGroupData<-downloadHandler(filename = function(){
    paste('data-',Sys.Date(),'.csv',sep='')},
    content = function(con){
      inputtmp<- hot_to_r(req(input$hot_gsummary))
      write.csv(inputtmp,con)
    }
  )
  #......Download Prob Plot Handler-----
  output$downloadProbPlot<-downloadHandler(
    filename = function() {
      "plotProb.pdf"
    },
    content = function(file) {
      file.copy("plotProb.pdf", file, overwrite=TRUE)
    }
  )
  #......Download Prob Data Handler-----
  output$downloadProbData<-downloadHandler(filename = function(){
    paste('data-',Sys.Date(),'.csv',sep='')},
    content = function(con){
      inputtmp<- hot_to_r(req(input$hot_prob_summary))
      #inputtmp<- hot_to_r(inputhot_prob_summary)
      write.csv(inputtmp,con)
    }
  )
  #......DOWNLOAD AVAILABLE PROB PLOT----
  output$downloadAvailableProbPlot<-downloadHandler(
    filename = function() {
      "plotAvaiProb.pdf"
    },
    content = function(file) {
      file.copy("plotAvaiProb.pdf", file, overwrite=TRUE)
    }
  )
  #......DOWNLOAD AVAILABLE PROB DATA----
  output$downloadAvailableProbData<-downloadHandler(filename = function(){
    paste('data-',Sys.Date(),'.csv',sep='')},
    content = function(con){
      inputtmp<- hot_to_r(req(input$hot_available_prob_summary))
      write.csv(inputtmp,con)
    }
  )
  #....OBSERVE EVENT FOR PROB PAGE----
  #......OBSERVE EVENT cSavingProb to Save new Prob click----
  observeEvent(req(input$cSavingProb), {
    #Prepare avaiable data
    HOT_Prob<-hot_to_r(req(input$hot_prob_sel))
    HOT_Summary<-hot_to_r(req(input$hot_prob_summary))
    SubSelectionVar<-input$idProbParameter
    SimSheet<-read.xlsx(SimFile,sheetName = "Sim")
    SimSheet$ID<-as.numeric(SimSheet$ID)
    SimSheet$BaseDisease<-as.character(SimSheet$BaseDisease)
    SimSheet$Name<-as.character(SimSheet$Name)
    SimSheet$Adjustment<-as.character(SimSheet$Adjustment)
    SimSheet$Source<-as.character(SimSheet$Source)
    SimSheet$NumberOfIterations<-as.numeric(SimSheet$NumberOfIterations)
    SimSheet$File<-as.character(SimSheet$File)
    NextID<-max(SimSheet$ID)+1
    NextFile=paste0("Sim/",NextID,".xlsx")
    ProbDisName<-as.character(input$txtNewProbDisease)
    MaxRound<-input$inpMaxRound
    BaseDisease<-input$selDiseaseProb
    vdis<-strsplit(BaseDisease,". ")
    RootDiseaseName<-as.character(unlist(vdis)[2])
    SourceFileName<-paste0("Sub/",unlist(vdis)[1],".xlsx")
    SelVar<-paste(SubSelectionVar, collapse=',' )
    TotalSheet<-rbind(SimSheet,c(NextID,RootDiseaseName,ProbDisName,MaxRound,SelVar,SourceFileName,NextFile))
    #Write master file
    write.xlsx(TotalSheet,file=SimFile,sheetName = "Sim",row.names = FALSE,append = F)
    #write sim file
    write.xlsx(HOT_Summary,file=NextFile,sheetName = "Result",row.names = FALSE,append = F)
    write.xlsx(HOT_Prob,file=NextFile,sheetName = "Parameters",row.names = FALSE,append = T)
    shinyjs::alert("Saving file:Done")
  })
  
  #....OBSERVE EVENT FOR Current BOD PAGE----
  #......Observe event cAction click----
  observeEvent(req(input$cAction), {
    shinyjs::alert("Calculation has been started, please wait for a few minutes.")
    if(!is.null(input$isCollapsecTable) && input$isCollapsecTable){
      #js$collapse("cTableBox")
    }
    if(!is.null(input$isCollapsecPlot) && input$isCollapsecPlot){
      #js$collapse("cPlotBox")
    }
    vdis<-strsplit(input$selDisease,". :")
    RootDiseaseName<-as.character(unlist(vdis)[2])
    SourceFileName<-paste0("Original/",unlist(vdis)[1],".xlsx")
    res<-BODCalculation(RootDiseaseName,"Country","","","","",SourceFileName)
    output$DALYPlot<-renderPlot({
      res[2]
    })
    output$hot_summary <- renderRHandsontable({
      OutRes<-as.data.frame(res[1])
      OutRes[,3]<-round(OutRes[,3],0)
      OutRes[,4]<-round(OutRes[,4],0)
      OutRes[,5]<-round(OutRes[,5],0)
      rhandsontable(as.data.frame(OutRes),readOnly = TRUE) %>% hot_cols(format = "#,#.#") 
    })
    
  })
  #....OBSERVE EVENT FOR NEW BOD PAGE----
  #......Observe event NewDisease name----
  shinyjs::disable('NewDiseaseFile')
  output$NewDiseaseResponseText<-renderText("<font size=4 color=red><b>Please type disease name to check availibility first then upload excel.</b></font>")
  observeEvent(input$NewDiseaseName, {
    if(!is.null(input$NewDiseaseName) && input$NewDiseaseName != "" ){
      output$NewDiseaseResponseText<-renderText("<font size=4 color=green><b>Checking disease name......</b></font>")
      MasterSheet<- read.xlsx(MasterFile, sheetName = "Master")
      if(sum(tolower(MasterSheet$Disease) == tolower(input$NewDiseaseName))>0){
        output$NewDiseaseResponseText<-renderText(paste0("<font size=4 color=red><b>\"",input$NewDiseaseName,"\" - Duplicate disease name.</b></font>"))
        shinyjs::disable('NewDiseaseFile')
      }else{
        output$NewDiseaseResponseText<-renderText(paste0("<font size=4 color=green><b>\"", input$NewDiseaseName,"\" is available. You are able to upload .xlsx file now.</b></font>"))
        shinyjs::enable('NewDiseaseFile')
      }
    }else{
      shinyjs::disable('NewDiseaseFile')
      output$NewDiseaseResponseText<-renderText("<font size=4 color=red><b>Please type disease name to check availibility first then upload excel.</b></font>")
    }
  })
  #......Observe event NewDisease File----
  observeEvent(req(input$NewDiseaseFile), {
    MasterSheet<- read.xlsx(MasterFile, sheetName = "Master")
    nextID<-max(as.integer(MasterSheet$ID))+1
    NewFlName<-paste0(nextID,".xlsx",sep="")
    nextID<-max(as.integer(MasterSheet$ID))+1
    file.copy(input$NewDiseaseFile$datapath,paste0("Original/",NewFlName))
    ResText<-paste("<span style=\"display: inline-block;padding: 5px; background-color: palegreen;color:blue\">Success! The ",input$NewDiseaseName,"'s data was uploaded completely. You may upload another disease with another name.","</span>" )
    #Update Excel
    MasterSheet$Disease<-as.character(MasterSheet$Disease)
    MasterSheet$File<-as.character(MasterSheet$File)
    MasterSheet$ID<-as.integer(MasterSheet$ID)
    MasterSheet<-rbind(MasterSheet,c(nextID,input$NewDiseaseName,NewFlName))
    write.xlsx(MasterSheet,file="Original/master.xlsx",sheetName = "Master",row.names = FALSE,append = F)
    output$NewDiseaseResponseText<-renderText(ResText)
    shinyjs::alert("UPLOAD COMPLETE")
  })
  #....OBSERVE EVENT FOR NEW PBOD PAGE----
  #js$collapse("pdwBox")
  #js$collapse("pdurationBox")
  #js$collapse("ptransitionBox")
  #js$collapse("pActionBox")
  #js$collapse("pPlotBox")
  #js$collapse("pTableBox")
  #......Observe event pLoadData name----  
  observeEvent(req(input$pLoadData), 
               {
                 if(input$selpDisease=="Select disease"){
                   shinyjs::alert("Please select the disease name first")
                 }else{
                   if(input$pLevelName==''){
                     shinyjs::alert("Please give the Region or province's detail first")
                   }else{
                     #Complete data
                     #js$collapse("pdwBox")
                     #js$collapse("pdurationBox")
                     #js$collapse("ptransitionBox")
                     #js$collapse("pActionBox")
                     tmpdis<-input$selpDisease
                     vdis<-strsplit(tmpdis," :")
                     FlName<-paste0("Original/",unlist(vdis)[1],"xlsx")
                     TransSheet  <- read.xlsx(FlName, sheetName = "Trans")
                     YLDSheet  <- read.xlsx(FlName, sheetName = "YLD")
                     DWSheet  <- read.xlsx(FlName, sheetName = "DW") 
                     DurationSheet  <- read.xlsx(FlName, sheetName = "Duration") 
                     ShortWestTableMale<- read.xlsx(FlName, sheetName = "ShortWestTableMale")
                     ShortWestTableFemale<- read.xlsx(FlName, sheetName = "ShortWestTableFemale")
                     #........HOT TABLE LOAD DATA----  
                     output$hot_ptransition <- renderRHandsontable({
                       rhandsontable(TransSheet,readOnly = FALSE) %>% 
                         hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
                     })
                     output$hot_pdw <- renderRHandsontable({
                       rhandsontable(DWSheet,readOnly = FALSE) %>% 
                         hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
                     })
                     output$hot_pduration <- renderRHandsontable({
                       rhandsontable(DurationSheet,readOnly = FALSE) %>% 
                         hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
                     })
                   }
                 }
               }
  )
  #......Observe event pAction for calculation----  
  observeEvent(req(input$pAction), {
    shinyjs::alert("Calculation has been started, please wait for a few minutes.")
    #js$collapse("pPlotBox")
    #js$collapse("pTableBox")
    vdis<-strsplit(input$selpDisease,". :")
    RootDiseaseName<-as.character(unlist(vdis)[2])
    SourceFileName<-paste0("Original/",unlist(vdis)[1],".xlsx")
    res<-BODCalculation(RootDiseaseName,input$selpLevel,input$pLevelName,"","","",SourceFileName)
    output$pDALYPlot<-renderPlot({
      res[2]
    })
    output$hot_psummary <- renderRHandsontable({
      rhandsontable(as.data.frame(res[1]),readOnly = TRUE) %>% 
        hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
    })
  })
  #....OBSERVE EVENT FOR Grouping PAGE----
  #......OBSERVE EVENT FOR Create Grouping button----
  observeEvent(req(input$cCreateGroup), {
    #js$collapse("pGroup")
    #print(input$idListDisease)
    tmpStrDiseases<-input$idListDisease
    DiseaseNameCol<-""
    DiseaseNumCol<-""
    for(x in tmpStrDiseases){
      res<-strsplit(x,". :")
      DiseaseNameCol<-paste0(DiseaseNameCol,res[[1]][2],":,:")
      DiseaseNumCol<-paste0(DiseaseNumCol,res[[1]][1],":,:")
    }
    #GroupFile<-"Original/group.xlsx"
    OriginalMainSheet<-read.xlsx(GroupFile, sheetName = "Main")
    nextID<-max(as.integer(OriginalMainSheet$ID))+1
    #str(OriginalMainSheet)
    OriginalMainSheet$ID<-as.numeric(OriginalMainSheet$ID)
    OriginalMainSheet$GroupName<-as.character(OriginalMainSheet$GroupName)
    OriginalMainSheet$DiseasesName<-as.character(OriginalMainSheet$DiseasesName)
    OriginalMainSheet$DiseasesID<-as.character(OriginalMainSheet$DiseasesID)
    OriginalMainSheet$Level<-as.character(OriginalMainSheet$Level)
    OriginalMainSheet$LevelName<-as.character(OriginalMainSheet$LevelName)
    #str(OriginalMainSheet)
    MyLevel<-input$selNewDiseaseGroupLevel
    print(paste("MyLevel=",MyLevel))
    MyLevel<-as.character(MyLevel)
    MyLevelName<-input$txtNewDiseaseGroupLevelName
    MyLevelName<-as.character(MyLevelName)
    NewRow<-c(nextID,input$txtNewDiseaseGroupName,DiseaseNameCol,DiseaseNumCol,MyLevel,MyLevelName)
    OriginalMainSheet<-rbind(OriginalMainSheet,NewRow)
    OriginalMainSheet$ID<-as.numeric(OriginalMainSheet$ID)
    OriginalMainSheet <-OriginalMainSheet[order(OriginalMainSheet$ID),]
    #ID	GroupName	DiseasesName	DiseasesID	Level	LevelName
    write.xlsx(OriginalMainSheet, file=GroupFile, sheetName="Main", row.names=FALSE)
    GroupSheet<- read.xlsx(GroupFile, sheetName = "Main")
    ListName<-GroupSheet %>% mutate(LineItem=paste0(ID,". ",GroupName," level:",Level," ",LevelName))
    ListName$LineItem<-gsub("NA","",ListName$LineItem)
    updateSelectInput(session, "selDiseaseGroup", 
                      choices = ListName$LineItem,
                      selected = NULL)
  })
  #......OBSERVE EVENT FOR Selection of group level----
  observeEvent(req(input$selNewDiseaseGroupLevel), {
    MyLevel<-input$selNewDiseaseGroupLevel
    print(paste("MyLevel=",MyLevel))
    SubFile
    MasterSheet<- read.xlsx(SubFile, sheetName = "Sub")
    MasterSheet <- MasterSheet %>% filter(Level==MyLevel) %>% mutate(Detail = paste0(ID,'. :',Disease," ",Level," ",Name))
    updateSelectInput(session, "idListDisease", 
                      choices = MasterSheet$Detail,
                      selected = NULL)
    #Region, Province -> Sub.xlsx file, else 
  })
  #....OBSERVE EVENT FOR AvailableDiseaseProb PAGE----
  #......OBSERVE EVENT FOR AvailableDisease Prob update----
  observeEvent(req(input$selAvailableDiseaseProb), {
    tmpdis<-input$selAvailableDiseaseProb
    vdis<-strsplit(tmpdis,". ")
    IDNum<-unlist(vdis)[1]
    IDNum<-as.integer(IDNum)
    SimSheet<-read.xlsx(SimFile,sheetName = "Sim")
    TargetRow<-SimSheet %>% filter(ID==IDNum)
    NumOfIterations<-TargetRow$NumberOfIterations
    output$outMaxRound<- renderText(paste0(NumOfIterations," rounds."))
    if(tmpdis !="A" && tmpdis != "Please select from below"){
      ResFile<-as.character(TargetRow$File)
      SourceFile<-TargetRow$Source
      Result<-read.xlsx(ResFile,sheetName = "Result")
      output$hot_available_prob_summary <- renderRHandsontable({
        rhandsontable(as.data.frame(Result,readOnly = TRUE)) %>% 
          hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      })
      sumDF<-Result
      #......PREPARE PLOT FOR AvailableDisease Prob update----
      output$probAvailableDALYPlot<-renderPlot({
        p1<-sumDF%>% ggplot(aes(y=YLD_0_0,x=Sex,color=Sex)) + geom_jitter()
        p2<-sumDF%>% ggplot(aes(y=YLD_3_0,x=Sex,color=Sex)) + geom_jitter()
        p3<-sumDF%>% ggplot(aes(y=YLD_3_1,x=Sex,color=Sex)) + geom_jitter()
        p4<-sumDF%>% ggplot(aes(y=YLL_0_0,x=Sex,color=Sex)) + geom_jitter()
        p5<-sumDF%>% ggplot(aes(y=YLL_3_0,x=Sex,color=Sex)) + geom_jitter()
        p6<-sumDF%>% ggplot(aes(y=YLL_3_1,x=Sex,color=Sex)) + geom_jitter()
        p7<-sumDF%>% ggplot(aes(y=DALY_0_0,x=Sex,color=Sex)) + geom_jitter()
        p8<-sumDF%>% ggplot(aes(y=DALY_3_0,x=Sex,color=Sex)) + geom_jitter()
        p9<-sumDF%>% ggplot(aes(y=DALY_3_1,x=Sex,color=Sex)) + geom_jitter()
        figure <- ggarrange(p1, p2, p3,p4,p5,p6,p7,p8,p9,
                            labels = c("0,0", "3,0", "3,1"),
                            ncol = 3, nrow = 3)
        ggsave("plotAvaiProb.pdf", figure)
        figure
      })
    }
  })
  #....OBSERVE EVENT FOR GROUP PAGE----
  #......OBSERVE EVENT FOR RENDER GROUP RESULTs----
  observeEvent(req(input$cRenderGroup), {
    GroupSheet<- read.xlsx(GroupFile, sheetName = "Main")
    tmpdis<-input$selDiseaseGroup
    vdis<-strsplit(tmpdis,". ")
    IDNum<-unlist(vdis)[1]
    IDNum<-as.integer(IDNum)
    TargetRow<-GroupSheet %>% filter(ID==IDNum)
    SubFolder<-paste0(TargetRow$Level,"/")
    tmpdis<-as.character(TargetRow$DiseasesID)
    vdis<-strsplit(tmpdis,":,:")
    TotalFiles<-length(unlist(vdis[1]))
    #print(unlist(vdis[1]))
    ArrayFile<-unlist(vdis[1])
    OverAll<-""
    ALL<-data.frame(Sex=character(),Age=character(),YLD_0_0=double())
    for(v in 1:TotalFiles){
      FlName<-paste0("result/",SubFolder,ArrayFile[v],".xlsx")
      print(FlName)
      ResultSheet<- read.xlsx(FlName, sheetName = "Main")
      if(v==1){
        #names(ResultSheet)<-c("Age","Sex","YLD_0_0")
        OverAll<-ResultSheet
        ALL<-ResultSheet
      }else{
        ResultSheet[is.na(ResultSheet)] <- 0
        ALL[is.na(ALL)] <- 0
        OverAll<-data.frame(OverAll,ResultSheet[,3:11])
        ALL$YLD_0_0<-ALL$YLD_0_0 + ResultSheet$YLD_0_0
        ALL$YLD_3_0<-ALL$YLD_3_0 + ResultSheet$YLD_3_0
        ALL$YLD_3_1<-ALL$YLD_3_1 + ResultSheet$YLD_3_1
        ALL$YLL_0_0<-ALL$YLL_0_0 + ResultSheet$YLL_0_0
        ALL$YLL_3_0<-ALL$YLL_3_0 + ResultSheet$YLL_3_0
        ALL$YLL_3_1<-ALL$YLL_3_1 + ResultSheet$YLL_3_1
        ALL$DALY_0_0<-ALL$DALY_0_0 + ResultSheet$DALY_0_0
        ALL$DALY_3_0<-ALL$DALY_3_0 + ResultSheet$DALY_3_0
        ALL$DALY_3_1<-ALL$DALY_3_1 + ResultSheet$DALY_3_1
      }
      #OverAll<-data.frame(OverAll,ResultSheet)
    }
    #print(ALL)
    MeltedDF = melt(ALL, id.vars = c("Sex", "Age"),
                    measure.vars = c("YLD_0_0", "YLD_3_0", 
                                     "YLD_3_1","YLL_0_0","YLL_3_0",
                                     "YLL_3_1","DALY_0_0","DALY_3_0","DALY_3_1"))
    #print(MeltedDF)
    #........Create Group plot----
    MyPlot<-ggplot(data=MeltedDF, aes(x=Sex, y=value, fill=Age)) + 
      geom_bar(stat="identity") + ggtitle("Fig 1: YLD,YLL,DALY plot")+ 
      theme_minimal()+ rremove("xlab")+ rremove("x.grid")+
      ylab("DALYs")+
      facet_wrap(~variable, scales = "free_y",ncol = 3)
    ggsave("plotGroup.pdf", MyPlot)
    output$gDALYPlot<-renderPlot({
      MyPlot
    })
    output$hot_gsummary <- renderRHandsontable({
      rhandsontable(as.data.frame(ALL,readOnly = TRUE)) %>% 
        hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
    })
  })
  #....OBSERVE EVENT FOR Probabilistic PAGE----
  #......OBSERVE EVENT FOR Probabilistic PAGE preparation----
  observeEvent(req(input$selDiseaseProb), {
    if(input$selDiseaseProb!="A"){
      arrstr<-strsplit(input$selDiseaseProb, ". ")
      SubFileLocation<-paste0("Sub/",arrstr[[1]][1],".xlsx")
      print(SubFileLocation)
      TransSheet  <- read.xlsx(SubFileLocation, sheetName = "Trans")  
      AllVar<-colnames(TransSheet)
      print(str(AllVar))
      SelectedVar<-AllVar[3:length(AllVar)]
      updateSelectInput(session, "idProbParameter", 
                        choices =SelectedVar,
                        selected = NULL)
      #Create parameter input
      output$hot_prob_sel <- renderRHandsontable({
        SubSelectionVar<-input$idProbParameter
        All_selCol <-data.frame()
        TargetCols<-TransSheet[,1:2]
        for (eVar in SubSelectionVar){
          SECOL<-paste0(eVar,"_SE")
          ALPHACOL<-paste0(eVar,"_AP")
          BETACOL<-paste0(eVar,"_BT")
          selCol <- TransSheet %>% select(eVar) %>% mutate(!!SECOL := rep(0.01,times=16))
          TargetCols<-cbind(TargetCols,selCol)
        }
        rhandsontable(TargetCols,readOnly = FALSE) %>% hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      })
    }
  })
  #......OBSERVE EVENT FOR Click Simulation ----
  observeEvent(req(input$cSimulation), {
    if(length(input$idProbParameter)>0){
      shinyjs::alert("Calculation has been started, please wait for a few minutes.")
      MaxRound<-input$inpMaxRound
      ProbTab<-hot_to_r(req(input$hot_prob_sel))
      selCol<-seq(1,ncol(ProbTab),2)
      ProbTabSel<-ProbTab[,selCol] %>% select(-Age)
      AllSE<- ProbTab %>% select(contains("_SE"))
      allv<-names(ProbTabSel)
      #........Simulation step1 : replicate TransSheet----
      arrstr<-strsplit(input$selDiseaseProb, ". ")
      SubFileLocation<-paste0("Sub/",arrstr[[1]][1],".xlsx")
      TransSheet  <- read.xlsx(SubFileLocation, sheetName = "Trans")  
      sim_list = replicate(n = MaxRound,expr = TransSheet ,simplify = F)
      #........Simulation step2 : Column bind data to TransSheet----
      tAll<-cbind(TransSheet,AllSE)
      for( k in 1:length(allv)){
        VarName<-allv[k]
        vAlp<-paste0(VarName,"_alpha")
        vBet<-paste0(VarName,"_beta")
        tSend<-tAll %>% select(mu=VarName,variance=paste0(VarName,"_SE"))
        tAll <- tAll %>% mutate(!!vAlp:=estBetaParams(tSend$mu,tSend$variance)$alpha,!!vBet:=estBetaParams(tSend$mu,tSend$variance)$beta)
        Param<-tAll %>% select(alp=paste0(VarName,"_alpha"),bet=paste0(VarName,"_beta"))
        for(i in 1:16){
          column_id<-which(colnames(TransSheet)==VarName)
          tmpa<-rbeta(MaxRound, Param$alp[i], Param$bet[i], ncp = 0)
          for(j in 1:MaxRound){
            sim_list[[j]][i,column_id]=tmpa[j]
          }
        }
      }
      #........Simulation step 3: Run All DALYs----
      res<-BODCalculation(input$txtNewProbDisease,"Country","","",sim_list,"Probabilistic",SubFileLocation)
      #........Simulation step 4: Collect all DALYs----
      sumDF<-data.frame()
      for(i in 1:length(res)){
        tmpRes<-res[[i]]
        SumDat<-tmpRes %>% select(-Age) %>% group_by(Sex) %>% summarise_all(funs(sum))
        if(i==1){
          sumDF<-SumDat
        }else{
          sumDF<-rbind(sumDF,SumDat)
        }
      }
      #........Simulation step 5: Print out DALYs----
      output$probDALYPlot<-renderPlot({
        p1<-sumDF%>% ggplot(aes(y=YLD_0_0,x=Sex,color=Sex)) + geom_jitter()
        p2<-sumDF%>% ggplot(aes(y=YLD_3_0,x=Sex,color=Sex)) + geom_jitter()
        p3<-sumDF%>% ggplot(aes(y=YLD_3_1,x=Sex,color=Sex)) + geom_jitter()
        p4<-sumDF%>% ggplot(aes(y=YLL_0_0,x=Sex,color=Sex)) + geom_jitter()
        p5<-sumDF%>% ggplot(aes(y=YLL_3_0,x=Sex,color=Sex)) + geom_jitter()
        p6<-sumDF%>% ggplot(aes(y=YLL_3_1,x=Sex,color=Sex)) + geom_jitter()
        p7<-sumDF%>% ggplot(aes(y=DALY_0_0,x=Sex,color=Sex)) + geom_jitter()
        p8<-sumDF%>% ggplot(aes(y=DALY_3_0,x=Sex,color=Sex)) + geom_jitter()
        p9<-sumDF%>% ggplot(aes(y=DALY_3_1,x=Sex,color=Sex)) + geom_jitter()
        figure <- ggarrange(p1, p2, p3,p4,p5,p6,p7,p8,p9,
                            labels = c("0,0", "3,0", "3,1"),
                            ncol = 3, nrow = 3)
        ggsave("plotProb.pdf", figure)
        figure
      })
      #........Simulation step 6: Update table DALYs----
      output$hot_prob_summary <- renderRHandsontable({
        rhandsontable(sumDF,readOnly = TRUE) %>% hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      })
    }else{
      shinyjs::alert("You need to select at least one parameters to perform calculation!")
    } 
  })
}#For server section


shinyApp(ui, server)