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
library(hutils)
library(stringi)
library(ggpubr)
library(ggrepel)
library(tidyverse)
library(xlsx)
library(V8)
#source("YLLcode.R")
#rsconnect::setAccountInfo(name='dalysthaitool', token='A62E515AF3CE1B3E1B9E129636F59BA3', secret='9FXZpmvyvpZuq5gtu4RFnQUOsZQbECXHmWhFMaEg')
#General Section----
#..parameters and file location----
#options(warn=-1)
DWSheet  <- data.frame()
DurationSheet  <- data.frame()
MasterFile<-"Original/master.xlsx"
SubFile<-"Original/sub.xlsx"
SimFile<-"Original/sim.xlsx"
GroupFile<-"Original/group.xlsx"
tmpFlname<-""
OriginalFlname<-""
#..General functios----
#....Javascript enable function----
# jscode <- "shinyjs.collapse = function(boxid) {
#   $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
# }"
OriginalPassCode<-"1"

#....Collapse function----
# collapseInput <- function(inputId, boxId) {
#   tags$script(
#     sprintf(
#       "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
#       boxId, inputId
#     ),
#     sprintf(
#       "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
#       boxId, inputId
#     )
#   )
# }
#....Estimate Beta params function----
estBetaParams <- function(mu, var) {
  # alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  # beta <- alpha * (1 / mu - 1)
  # return(params = list(alpha = alpha, beta = beta))
}
#UI Section----
#..UI Dashboard----
ui <- fluidPage(
                useShinyjs(),
                dashboardPage(skin = "yellow",
                    dashboardHeader(title = "PBOD v 4.91",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Version 4.91 Update :",
                                                   message = "Add confirmation to overwrite country base file",
                                                   icon = icon("life-ring"),
                                                   time = "2021-06-24")
                                    )
                    ), 
                    #useShinyjs(),
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
                      tabItems(
                         #....TAB: NEWBOD----------------------------------------------
                        tabItem(tabName = "newbod",
                                fluidRow
                                (
                                  box(id = "uploadnew", title = "Upload new disease:",
                                      status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                        textInput("NewDiseaseName", "Disease name:",""),
                                        fileInput("NewDiseaseFile", "Choose .XLSX File",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".xlsx")),
                                      checkboxInput("chkSpecialModule", "Need special module", value = FALSE, width = NULL),
                                      textInput("txtSpecialModuleName", "Module name:",""),
                                        actionButton("cUploadNewDisease", "Upload Data",align="center", icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        htmlOutput("NewDiseaseResponseText")
                                  )
                                  
                                  
                                )
                        )
                        ) #tabItem
                      )#End of Tab item
                    )#End of Dashboard body
)#End of Dashboard
#Server Section----
server <- function(session,input, output) {
  #..Essential BOD Function----
  
  
  
  
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
  #....OBSERVE EVENT FOR PBOD PAGE----
  #....OBSERVE EVENT FOR CURRENT PBOD AVAILABLE PAGE----
  #....OBSERVE EVENT FOR CURRENT BOD PAGE----
  #....OBSERVE EVENT FOR PROB PAGE----
  #......OBSERVE EVENT cSavingProb to Save new Prob click----
  #....OBSERVE EVENT FOR Current BOD PAGE----
  #......Observe event cAction click----
  #....OBSERVE EVENT FOR NEW BOD PAGE----
  #......Observe event NewDisease name----
  observeEvent(input$NewDiseaseName, {
    if(!is.null(input$NewDiseaseName) && input$NewDiseaseName != "" ){
      output$NewDiseaseResponseText<-renderText("<font size=4 color=green><b>Checking disease name......</b></font>")
      #MasterSheet<- read.xlsx(MasterFile, sheetName = "Master")
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
  observeEvent(req(input$cUploadNewDisease), {
    print("FILE NAME:")
    print(input$NewDiseaseFile$datapath)
    if(!is.null(input$NewDiseaseFile$datapath)){
      req(input$NewDiseaseFile)
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
    }else{
      shinyjs::alert("EMPTY FILE, cancle uploading")
    }
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