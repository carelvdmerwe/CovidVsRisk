Install.packagesTriplot <- function()
{
  list.of.packages <- c("readxl","shiny", "shinydashboard", "shinyWidgets","Hmisc","plotfunctions","processx","lattice",
                        "survival","ggplot2","Formula","stringr","LW1949","utils")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  library(readxl)
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(Hmisc)
  library(plotfunctions)
  library(processx)
  library(lattice)
  library(survival)
  library(ggplot2)
  library(Formula)
  library(stringr)
  library(LW1949)
  library(utils)

  
  #Other functions part of UBbipl
  PCAbipl_C <<- source("source/PCAbipl_C.R")$value
  drawbipl.bagalpha_C <<- source("source/drawbipl.bagalpha_C.R")$value
  compute.bagplot_C <<- source("source/compute.bagplot_C.R")$value
  indmat <<- source("source/indmat.R")$value
  Draw.line2 <<- source("source/Draw.line2.R")$value
  Draw.onecmline <<- source("source/Draw.onecmline.R")$value
  Plot.marker.new <<-source("source/Plot.marker.new.R")$value
  
  #Loading of data
  coviddata <<- read_xlsx("COVID-19-geographic-disbtribution-worldwide-2020-12-08.xlsx")[,c(1,5,6,9)]
  #COVID-19-geographic-disbtribution-worldwide-2020-12-08
  #update available: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

  riskdata <<- read_xlsx("RiskIndices.xlsx",na = "#N/A")
  ctrycodes <<- as.matrix(unique(riskdata[,1]))
  ctrycodes <<- ctrycodes[!is.na(ctrycodes),]
  ctrycodes <<- ctrycodes[order(ctrycodes)]
  contcodes <<- ctrycodes
  Countrydata <<- list()
  RiskdataMat <<- as.data.frame(matrix(rep(0,29*length(ctrycodes)),ncol=29))
  rownames(RiskdataMat) <<- noquote(ctrycodes)
  colnames(RiskdataMat) <<- t(riskdata[1:29,7])
  for (i in 1:length(ctrycodes))
  {
    Countrydata[[i]] <<- coviddata[!is.na(coviddata[,4])&coviddata[,4]==ctrycodes[i],]
    Countrydata[[i]] <<- Countrydata[[i]][rev(1:dim(Countrydata[[i]])[1]),]
    Countrydata[[i]] <<- cbind(Countrydata[[i]], cumsum(Countrydata[[i]][,2]),cumsum(Countrydata[[i]][,3]))
    colnames(Countrydata[[i]]) <<- c("Date","Cases","Deaths","Code", "CumCase","CumDeaths")
    names(Countrydata)[i] <<- noquote(ctrycodes[i])
    RiskdataMat[i,] <<- as.matrix(riskdata[riskdata$CountryCode==ctrycodes[i],6])
    contcodes[i] <<- as.matrix(riskdata[riskdata$CountryCode==ctrycodes[i],8][1,1])
  }
  
  #1.  NA	Country Risk Index
  #2.  CRI	Short-Term Political Risk Index
  #3.  STPR	Policy-Making Process
  #4.  STPR	Social Stability
  #5.  STPR	Security/External Threats
  #6.  STPR	Policy Continuity
  #7.  CRI	Long-Term Political Risk Index
  #8.  LTPR	Characteristics of Polity
  #9.  LTPR	Characteristics of Society
  #10. LTPR	Scope of State
  #11. LTPR	Policy Continuity
  #12. CRI	Short-Term Economic Risk Index
  #13. STER	Economic Growth
  #14. STER	Monetary Policy
  #15. STER	Fiscal Policy
  #16. STER	External Factors
  #17. STER	Financial Markets
  #18. CRI	Long-Term Economic Risk Index
  #19. LTER	Structural Characteristics
  #20. LTER	Economic Growth
  #21. LTER	Monetary Policy
  #22. LTER	Fiscal Policy
  #23. LTER	External Factors
  #24. LTER	Financial Markets
  #25. CRI	Operational Risk Index
  #26. OR	Logistics Risk Index
  #27. OR	Labour Market Risk Index
  #28. OR	Trade and Investment Risk Index
  #29. OR	Crime and Security Risk Index
  CRI <<- c(2,7,12,18,25)
  STPR <<- c(3,4,5,6)
  LTPR <<- c(8,9,10,11)
  STER <<- c(13,14,15,16, 17)
  LTER <<- c(19,20,21,22,23,24)
  ST <<- c(STPR,STER)
  LT <<- c(LTPR, LTER)
  OR <<- c(26,27,28,29)
  AllSub <<- c(STPR, LTPR, STER, LTER, OR)

  SCmin <<- 1
  SCmax <<- 50000
  SCstep <<- 1000
  DACmin <<- 1
  DACmax <<- 200
  DACstep <<- 10
  SDmin <<- 1
  SDmax <<- 50000
  SDstep <<- 100
  DADmin <<- 1
  DADmax <<- 200
  DADstep <<- 10

  colfun <<- colorRampPalette(c("green","orange","red"))
  options(scipen=9999999)
}

Install.packagesTriplot()

BinsFunc <- function(binsinputin,tempvisinin, alphain,freezein=FALSE)
{
  if (binsinputin == "equal.size") 
  {
    Binscut <- cut2(tempvisinin, g=alphain)
    Bins.legend1<-as.matrix(levels(Binscut))
    Binsnrs <- unique(as.numeric(unlist(str_extract_all(as.character(Bins.legend1),"\\d+\\.*\\d*"))))
  }
  
  if (binsinputin == "equal.width") 
  {
    Binscut <- cut2(tempvisinin, seq(from = 1,to = max(tempvisinin,na.rm = TRUE)+0.01, length.out = alphain+1))
    Bins.legend1<-as.matrix(levels(Binscut))
    Binsnrs <- unique(as.numeric(unlist(str_extract_all(as.character(Bins.legend1),"\\d+\\.*\\d*"))))
  }
  
  if (binsinputin == "equal.width.log") 
  {
    Binscut <- cut2(log(tempvisinin), seq(from =min(log(tempvisinin)-0.01,na.rm = TRUE),to = max(log(tempvisinin)+0.01,na.rm = TRUE), length.out = alphain+1))
    Bins.legend1<-as.matrix(levels(Binscut))
    Binsnrs <- exp(unique(as.numeric(unlist(str_extract_all(as.character(Bins.legend1),"\\d+\\.*\\d*")))))
  }
  
  list(Binscut=as.numeric(Binscut), Binsnrs=Binsnrs)
}

#Vis 1: NUMBER OF CASES X DAYS AFTER THE FIRST C CASES
#Vis 2: NUMBER OF DEATHS X DAYS AFTER THE FIRST D DEATHS								
#Vis 3: NUMBER OF CASES AT THE FIRST D DEATHS	
#Vis 4: NUMBER OF DEATHS AT THE FIRST C CASES			
#vIS 5: NUMBER OF DAYS TO REACH C CASES FROM FIRST CASE
#VIS 6: NUMBER OF DAYS TO REACH D DEATH FROM FIRST CASE
#VIS 7: NUMBER OF DAYS TO REACH D DEATH FROM FIRST DEATH

VisRes <- function(StartCase, DaysAfterCase, StartDeath, DaysAfterDeath, VisChoice)
{
  VisResOut <-matrix(rep(0,length(ctrycodes)),ncol=1)
  rownames(VisResOut) <- ctrycodes
  Vistemp <- lapply(Countrydata, function(x)
    {
    CaseInit <- x[x[,5]>=1,1][1]
    DeathInit <- x[x[,6]>=1,1][1]
    StartCaseDate <- x[x[,5]>=StartCase,1][1]
    StartDeathDate <- x[x[,6]>=StartDeath,1][1]
    EndCaseDate <- StartCaseDate+60*60*24*DaysAfterCase
    EndDeathDate <- StartDeathDate+60*60*24*DaysAfterDeath
    if(VisChoice==1) outp <- x[x[,1]>=EndCaseDate,5][1]
    if(VisChoice==2) outp <- x[x[,1]>=EndDeathDate,6][1]
    if(VisChoice==3) outp <- x[x[,6]>=StartDeath,5][1]
    if(VisChoice==4) outp <- x[x[,5]>=StartCase,6][1]
    if(VisChoice==5) outp <- StartCaseDate-CaseInit
    if(VisChoice==6) outp <- StartDeathDate-CaseInit
    if(VisChoice==7) outp <- StartDeathDate-DeathInit
    return(outp)
  }
  )
  VisResOut[,1] <- unlist(Vistemp)
  return(VisResOut)
}

ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title="COVID-19"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard",tabname="dashboard", icon=icon("dashboard"),startExpanded = TRUE,
                 checkboxInput(inputId="freeze", "Freeze legend", value = FALSE, width = NULL),
                 radioButtons(inputId = "Bins",label="Bins",
                              choices  = c("Equal size"="equal.size",
                                           "Equal width"="equal.width",
                                           "Equal width log"="equal.width.log"),
                              selected = "equal.size"),
                 radioButtons(inputId = "legend.scale",label="Legend Scale",
                              choices  = c("Log scale"="log.scale",
                                           "Normal scale"="norm.scale"),
                              selected = "log.scale"),
                radioButtons(inputId = "Conts", label ="Continents",
                             choices=c("World"="World",
                                       "Africa"="Africa",
                                       "America"="America",
                                       "Asia"="Asia",
                                       "Europe"="Europe",
                                       "Oceania"="Oceania"),
                             selected="World",inline=TRUE),
                sliderInput("topnum","Max number of countries on legend (for World)", min=1,max=191,value=10, step =1),
                pickerInput("specctry",label = "Specific countries to plot:", choices=ctrycodes,selected = NULL,multiple = TRUE, inline = FALSE),
            checkboxInput('returnpdf', 'Output to PDF?', FALSE),
              conditionalPanel(
                condition = "input.returnpdf == true",
                strong("PDF size (inches):"),
                sliderInput(inputId="w", label = "width:", min=3, max=20, value=10,ticks=F),
                downloadLink('pdflink')
                ))
        
      )
    ),
    dashboardBody(
      fluidRow(
        box(title="Biplot", status="primary",solidHeader= TRUE, 
            collapsible = TRUE,width=7,height=9,
            plotOutput("bipl",height="800px")
        ), 
        box(
          title="Legend", status="primary",solidHeader= TRUE,
          collapsible = TRUE,width=2,height=9,
          plotOutput("legend.bl",height="800px")
        ),
        
        box(
          title="Inputs",status = "warning",solidHeader= TRUE,
          collapsible = TRUE,width=3,
          radioButtons(inputId = "Visualisation", label ="Visualisation",
                       choices=c("Vis1: #Cases 'X' days after the first 'C' cases."="1",
                                 "Vis2: #Deaths 'X' days after the first 'D' deaths."="2",
                                 "Vis3: #Cases at the first 'D' deaths."="3",
                                 "Vis4: #Deaths at the first 'C' cases."="4",
                                 "Vis5: #Days to reach 'C' cases from first case."="5",
                                 "Vis6: #Days to reach 'D' deaths from the first case."="6",
                                 "Vis7: #Days to reach 'D' deaths from first death."="7"),
                       selected="1"),
          sliderInput("Startcase","Cases (C):",min=SCmin,max=SCmax,value=1000,step=SCstep, animate=TRUE, animationOptions(interval =60000, loop= TRUE)),
          sliderInput("Startdeath","Deaths (D):",min=SDmin,max=SDmax,value=100,step = SDstep,  animate=TRUE),
          sliderInput("Startdays","Days (X):",min=DACmin,max=DACmax,value=10,step=DACstep,  animate=TRUE),
          radioButtons(inputId = "varstoplot",label="Variables",
                       choices=c(  "Country Risk Index"="CRI",
                                  "Short Term Political Risk Index"="STPR",
                                  "Long Term Political Risk Index"="LTPR",
                                  "Short Term Economic Risk Index"="STER",
                                  "Long Term Economic Risk Index"="LTER",
                                  "Short Term Index"="ST",
                                  "Long Term Index"="LT",
                                  "Operational Risk Index"="OR",
                                  "All Indices"="AllSub")),
          sliderInput("alpha","Number of Groups",min=1,max=10,value=3),
          sliderInput("alphasize","Alpha-bag Level",min=0,max=0.99,value=0.95),
          sliderInput("numobsinbag","Number of samples requried for alphabag",min=1, max=1000,value=10),
          
        )
        
      )
    )
  )
)

biplotrun <- function(VisChoicein = input$Visualisation, binsinputin = input$Bins,alpha.sizein=input$alphasize, alphanuminin = input$alpha,
                      nobsinbagin = input$numobsinbag,  varstoplotin = input$varstoplot, 
                      SCin = input$Startcase, DACin = input$Startdays, SDin =  input$Startdeath, DADin = input$Startdays,
                      freezein = input$freeze, topnumin = input$topnum, specctryin = input$specctry, contsin =input$Conts)
  {

  
  #Inputs
  VisChoice <<- VisChoicein
  binsinput <<- binsinputin
  alpha.size <<-alpha.sizein
  alphanumin <<-alphanuminin
  nobsinbag <<- nobsinbagin
  conts <<- contsin
  
  varstoplot <<- varstoplotin
  topnum <<- topnumin
  SC <<- SCin
  DAC <<- DACin
  SD <<- SDin
  DAD <<-  DADin
  VisChoiceval <<- as.numeric(VisChoice)
  freeze <<- freezein
  specctry <<- specctryin

  varsinput <<-switch(varstoplot,
                      "CRI"=CRI,
                      "STPR"=STPR,
                      "LTPR"=LTPR,
                      "STER"=STER,
                      "LTER"=LTER,
                      "ST"=ST,
                      "LT"=LT,
                      "OR"=OR,
                      "AllSub"=AllSub)
  

  tempvis <<- VisRes(StartCase = SC, DaysAfterCase = DAC ,
                     StartDeath = SD, DaysAfterDeath = DAD,VisChoice = VisChoiceval) #first vis\
  rv$tempvis1 <<-tempvis
  if(all(is.na(tempvis))) stop()
  
  if(freeze == FALSE) tempbins <<- BinsFunc(binsinputin = binsinput, tempvisinin = tempvis,alphain = alphanumin,freezein = freeze)$Binscut
  if(freeze == TRUE) 
    {
    tempcutmax <<- max(max(Binsnrs,na.rm=TRUE),max(tempvis,na.rm=TRUE))
    tempbins <<- as.numeric(cut2(tempvis, cuts = c(1,Binsnrs[-c(1,length(Binsnrs))],tempcutmax)))
    }
  
  
  if(sum(is.na(tempbins))==0)
  {
    nbins <<- (1:dim(indmat(tempbins))[2])[apply(indmat(tempbins),2,sum) > nobsinbag]
    newcol <<- colfun(dim(indmat(tempbins))[2])
  }
  
  if(sum(is.na(tempbins))>0)
  {
    tempbins[is.na(tempbins)]<<-0
    newcol <<- c("#DCDCDC",colfun(alphanumin))[as.numeric(colnames(indmat(tempbins)))+1]
    nbins <<- (1:dim(indmat(tempbins))[2])[apply(indmat(tempbins),2,sum) > nobsinbag]
    nbins<<- nbins[!nbins==1]
  }
  
  if(length(nbins)==0) nbins <<- NULL
  
  if(is.null(specctry) & conts=="World") top10visnamesbipl <<- rownames(tempvis)[!is.na(tempvis)][order(tempvis[!is.na(tempvis)],decreasing=TRUE)]
  if(is.null(specctry) & conts!="World") top10visnamesbipl<<- rownames(tempvis)[contcodes==conts]
  if(!is.null(specctry)) top10visnamesbipl<<- specctry
  toplot <<- RiskdataMat[,varsinput]
  namestoplot <<- !is.na(match(rownames(toplot),top10visnamesbipl))
  
  tempbipl <<- PCAbipl_C(X = toplot,pos = "Hor" ,
                         main="",
                         offset = c(-1.5, 0.5,0,0.5),
                         ax.name.size=0.9,c.hull.n = nobsinbag, 
                         G=indmat(tempbins),colours=newcol,
                         pch.samples=rep(21,dim(indmat(tempbins))[2],bg="black",scale=FALSE),
                         alpha=alpha.size,specify.bags=NULL, marker.size = 0.7, lblplot = namestoplot, label.size = 0.6 )
  
  classes <<- unique(tempbins)
  for (j in 1:length(classes)) 
  {
    subz <<- tempbipl$Z[tempbipl$Z[,5]==classes[j]+1,]
    points(x = subz[,1], 
           y = subz[,2], pch = subz[,3], 
           col = newcol[classes[j]+1], cex = 1, bg = newcol[classes[j]+1])
  }
  
  for(b in nbins)
  {
    loopcoords <<- compute.bagplot_C(tempbipl$Z[,1:2][tempbipl$Z[,5]==b,], factor = 1,alph=alpha.size)$hull.fullloop
    polygon(loopcoords[,1],loopcoords[,2], density = 0, 
            col = newcol[b], lwd = 1, lty = 2)
  }
  
  biplottitle<-switch(VisChoice,
                      "1"=title(paste0("Visualisation ", VisChoice,": Number of cases ",DAC ," days after the first ",SC," cases.")),
                      "2"=title(paste0("Visualisation ", VisChoice,": Number of deaths ",DAD ," days after the first ",SD," deaths.")),
                      "3"=title(paste0("Visualisation ", VisChoice,": Number of cases at the first ",SD," deaths")),
                      "4"=title(paste0("Visualisation ", VisChoice,": Number of deaths at the first ",SC," cases.")),
                      "5"=title(paste0("Visualisation ", VisChoice,": Number of days to reach ",SC ," cases from first case.")),
                      "6"=title(paste0("Visualisation ", VisChoice,": Number of days to reach ",SD ," deaths from first case.")),
                      "7"=title(paste0("Visualisation ", VisChoice,": Number of days to reach ",SD ," deaths from first death.")))
}

legend.all <<- function(binsinput =input$Bins,lscalein =input$legend.scale,tempvisin = rv$tempvis1, freezein=input$freeze,
                        topnumin = input$topnum, specctryin = input$specctry, contsin = input$Conts){
  lscale <<- lscalein
  freeze <<- freezein
  topnum <<- topnumin
  specctry <<- specctryin
  conts <<- contsin
  
  if(freeze == FALSE) Binsnrs <<- BinsFunc(binsinput, tempvisin, alphanumin, freezein = freeze)$Binsnrs  
  
  Binsnrslog <- log(Binsnrs)
  BinsnrsStand <- (Binsnrs - 0)/(max(Binsnrs)-0)
  BinsnrsStandlog <- (Binsnrslog - min(Binsnrslog))/(max(Binsnrslog)-min(Binsnrslog))
  BinsnrsStand <- c(0,BinsnrsStand[-1])
  
  xl <- 1
  yb <- 1
  xr <- 1.8
  yt <- 40
  
  if(lscale =="log.scale")    BinsnrsStandtb <<- yb + BinsnrsStandlog*(yt-yb) 
  if(lscale =="norm.scale")    BinsnrsStandtb <<- yb + BinsnrsStand*(yt-yb) 
  tempbinsnr <<- c(0,Binsnrs)
  binstexttb <- seq(yb,yt,(yt-yb)/20)
  binstext <- round(seq(0,max(Binsnrs),length.out=21),0)
  binstextlog <- round(exp(seq(min(Binsnrslog),max(Binsnrslog),length.out=21)),0)
  tempbinstexttb <<- binstexttb
  tempbintext <<- binstext
  tempbinstextlog <<- binstextlog
  
  par(mar=c(0,0,0,0))
  plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,40),xaxt="n",yaxt="n",bty="n",asp=1)
  rect(
    xl,
    head(BinsnrsStandtb,-1),
    xr,
    tail(BinsnrsStandtb,-1),
    col=colfun(length(Binsnrs)-1)
  )
  
  if(is.null(specctry) & conts=="World")
  {
  topshow <- min(topnum, length(tempvisin[!is.na(tempvisin)]))
  top10visvals <<- tempvisin[!is.na(tempvisin)][order(tempvisin[!is.na(tempvisin)],decreasing=TRUE)][1:topshow]
  top10visnames <<- rownames(tempvisin)[!is.na(tempvisin)][order(tempvisin[!is.na(tempvisin)],decreasing=TRUE)][1:topshow]
  }
  
  
  if(is.null(specctry) & conts!="World")
  {
    top10visvals<<- tempvis[contcodes==conts]
    top10visnames<<- rownames(tempvis)[contcodes==conts]
  }
  
  if(!is.null(specctry))
  {
    top10visvals<<- tempvis[!is.na(match(rownames(tempvis),specctry))]
    top10visnames<<- specctry
  }
  
  if(lscale =="norm.scale")  
  {
    tempabctext <<- pretty(binstext,n=21)
    temppos <<- approx(x=binstext,y=binstexttb,xout=tempabctext)$y
    tempabctext <<- tempabctext[!is.na(temppos)]
    temppos <<- temppos[!is.na(temppos)]
    text(x=0.8,y=temppos, labels = format(tempabctext,nsmall=0,big.mark=","),adj=1, cex = 0.75)
    templegtextpos <<- approx(x=binstext,y=binstexttb,xout=top10visvals)$y
    if(length(top10visnames[!is.na(templegtextpos)])>0) text(x = 2, y=templegtextpos[!is.na(templegtextpos)], labels =top10visnames[!is.na(templegtextpos)],adj=0, cex = 0.75)
  }
  if(lscale =="log.scale") 
  {
    tempabctext <<- prettylog(binstextlog,lead=c(1,2,5),extra=10)
    temppos <<- approx(x=binstextlog,y=binstexttb,xout=tempabctext)$y
    tempabctext <<- tempabctext[!is.na(temppos)]
    temppos <<- temppos[!is.na(temppos)]
    text(x=0.8,y=temppos, labels = format(tempabctext,nsmall=0,big.mark=","),adj=1, cex = 0.75)
    templegtextpos <<- approx(x=binstextlog,y=binstexttb,xout=top10visvals)$y
    if(length(top10visnames[!is.na(templegtextpos)])>0) text(x = 2, y=templegtextpos[!is.na(templegtextpos)], labels =top10visnames[!is.na(templegtextpos)],adj=0, cex = 0.75)
  }
}

server <- function(input,output,session) 
{
  rv <<- reactiveValues()
  SCrv <<- reactive(input$Startcase)
  plotInput <<- reactive({
    if(input$returnpdf){
      pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$w)/10*8)
      par(fig=c(0.1,0.8,0,1))
      biplotrun(VisChoicein = input$Visualisation, binsinputin = input$Bins,alpha.sizein=input$alphasize, alphanuminin = input$alpha,
                nobsinbagin = input$numobsinbag,  varstoplotin = input$varstoplot, 
                SCin = input$Startcase, DACin = input$Startdays, SDin =  input$Startdeath, DADin = input$Startdays,
                freezein = input$freeze, topnumin = input$topnum, specctryin = input$specctry, contsin =input$Conts)
      par(fig=c(0.7,1,0,1),new=TRUE)
      legend.all(binsinput=input$Bins,lscalein =input$legend.scale,tempvisin = rv$tempvis1,
                 freezein = input$freeze, topnumin = input$topnum, specctryin = input$specctry, contsin = input$Conts)
      dev.off()
    }
    biplotrun(VisChoicein = input$Visualisation, binsinputin = input$Bins,alpha.sizein=input$alphasize, alphanuminin = input$alpha,
              nobsinbagin = input$numobsinbag, varstoplotin = input$varstoplot, 
              SCin = input$Startcase, DACin = input$Startdays, SDin =  input$Startdeath, DADin = input$Startdays,
              freezein = input$freeze, topnumin = input$topnum, specctryin = input$specctry, contsin =input$Conts)
  })
  
  
  output$bipl <-
    renderPlot(width="auto",height="auto",{ 
                plotInput()
                    }, res = 100)
  output$pdflink <- downloadHandler(
    filename <- "myplot.pdf",
    content <- function(file) {
      file.copy("plot.pdf", file)
    }
  )

  output$legend.bl <- renderPlot(width="auto",height="auto",
                               {legend.all(binsinput=input$Bins,lscalein =input$legend.scale,tempvisin = rv$tempvis1,
                                           freezein = input$freeze, topnumin = input$topnum, specctryin = input$specctry, contsin = input$Conts)
                              }, res = 100)
}

shinyApp(ui=ui, server = server)
