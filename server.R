# Author: Matt Watts
# Date: 10 Dec 2014
# Purpose: ParameterTestingTas web app server.R

require(shiny)
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
require(xtable)
library(foreach)
library(doMC)

registerDoMC(10)  # the number of CPU cores 

cat("\n")
cat(sMarxanDir)
cat("\n")

# initialise objects from R binary files
load(file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))
puoutline <<- puoutline
pulayer_ <<- pulayer_
pustatus_ <<- pustatus_

PrepareDisplay <- function()
{
    # prepare the map: pulayer object
    pulayer <<- pulayer_
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))
    # prepare the planning unit status object
    pustatus <<- pustatus_
}

PrepareDisplay()

PadInt <- function(iRunNumber)
{
  sFilename <- ""
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste0(sFilename,"0")
    }
  }
  sFilename <- paste0(sFilename,iRunNumber)  
  return(sFilename)
}
       
GetOutputFileext <- function(sMarxanDir,sParam)
# For the specified Marxan output file, return the file extension (.csv or .txt)
# Scan input.dat for the parameter,
# if value = 1, .dat, tab delimited, no header
# if value = 2, .txt, comma delimited (Qmarxan sets this value)
# if value = 3, .csv, comma delimited
{
  inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
  iParam <- which(regexpr(sParam,inputdat)==1)
  
  iValue <- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])
  
  if (iValue == 1)
  {
    return(".dat")
  }
  if (iValue == 2)
  {
    return(".txt")
  }
  if (iValue == 3)
  {
    return(".csv")
  }
}

GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste(sMarxanDir,"/output/output_r",sep="")  
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste(sFilename,"0",sep="")
    }
  }
  sFilename <- paste(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"),sep="")  
}

ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, sPUID)
# Imports the relevant contents of output files to the planning unit shape file dbf.
{
  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  pu_table <- sqldf(paste("SELECT ", sPUID, " from pu_table",sep=""))
  colnames(pu_table)[1] <- "PUID"
                    
  pu_table$PUID <- as.integer(pu_table$PUID)
  
  # load and prepare ssoln_table
  ssoln_table <- read.csv(paste(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN"),sep=""))
  colnames(ssoln_table)[1] <- "PUID"
  colnames(ssoln_table)[2] <- "SSOLN2"
  ssoln_table$SSOLN1 <- as.integer(iNumberOfRuns - ssoln_table$SSOLN2)
  ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)
  
  # join pu_table and ssoln_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")
  
  # load and prepare best_table
  best_table <- read.csv(paste(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST"),sep=""))
  best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
  best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")
  
  # join pu_table and best_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")
  
  # save the new pu_table
  colnames(pu_table)[1] <- sPUID
  write.dbf(pu_table,sPuShapeFileDbf)  
}

safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}

substrRight <- function(x, n)
{
    substr(x, nchar(x)-n+1, nchar(x))
}

shinyServer(function(input, output, session) {

    system("touch /var/shiny-server/www/CalibrationSensitivity_Tas_rev2/restart.txt")
    
    observe({
        ruserblm <<- as.numeric(input$userblm)
        cat(paste0("ruserblm ",ruserblm,"\n"))
    })
    
    observe({
        ruserspf <<- as.numeric(input$userspf)
        cat(paste0("ruserspf ",ruserspf,"\n"))
    })
    
    observe({
        rusertarg <<- as.numeric(input$usertarg)
        cat(paste0("rusertarg ",rusertarg,"\n"))
    })
    
    observe({
        cat("observe whichparam\n")

        if (input$whichparam == "BLM Calibration")
        {
            swhichparam <<- "BLM"
        }
        if (input$whichparam == "SPF Calibration")
        {
            swhichparam <<- "SPF"
        }
        if (input$whichparam == "Target Sensitivity")
        {
            swhichparam <<- "Targ"
        }
        
        itestinput <<- itestinput + 1
        updateNumericInput(session, "testinput", value = itestinput)
    })

    observe({
        cat("observe whichmap\n")

        iwhichmap <<- input$whichmap
        
        itestinput <<- itestinput + 1
        updateNumericInput(session, "testinput", value = itestinput)
    })

    observe({
        cat("observe whichrun\n")

        if (input$whichrun == "Best Solution")
        {
            swhichrun <<- "best"
        } else {
            if (input$whichrun == "Selection Frequency")
            {
                swhichrun <<- "ssoln"
            } else {
               swhichrun <<- substrRight(input$whichrun, nchar(input$whichrun)-4)
            }
        }    

        itestinput <<- itestinput + 1
        updateNumericInput(session, "testinput", value = itestinput)
    })
    
    observe({
        rRampBLMmin <<- input$rampBLMmin
        cat(paste0("rRampBLMmin ",rRampBLMmin,"\n"))
    })

    observe({
        rRampBLMmax <<- input$rampBLMmax
        cat(paste0("rRampBLMmax ",rRampBLMmax,"\n"))
    })
    
    observe({
        rRampSPFmin <<- input$rampSPFmin
        cat(paste0("rRampSPFmin ",rRampSPFmin,"\n"))
    })

    observe({
        rRampSPFmax <<- input$rampSPFmax
        cat(paste0("rRampSPFmax ",rRampSPFmax,"\n"))
    })
    
    observe({
        rtargetmin <<- input$targetmin
        cat(paste0("rtargetmin ",rtargetmin,"\n"))
    })

    observe({
        rtargetmax <<- input$targetmax
        cat(paste0("rtargetmax ",rtargetmax,"\n"))
    })

    runmarxan <- reactive({
        cat("runmarxan\n")

        if (input$mrun == 0)
        {
            imrun <<- 0
            cat("init mrun\n")
        }
        else
        {
            if (input$mrun > imrun)
            {
                imrun <<- input$mrun
                cat("mrun incremented\n")
        
                # set min, max, interval for value ramping
                if (swhichparam == "BLM")
                {
                    rMinimum <- safe_log(rRampBLMmin)
                    rMaximum <- safe_log(rRampBLMmax)
                    rInterval <- (rMaximum - rMinimum) / 9        
                    rValue <- rRampBLMmin
                }
                if (swhichparam == "SPF")
                {
                    rMinimum <- safe_log(rRampSPFmin)
                    rMaximum <- safe_log(rRampSPFmax)
                    rInterval <- (rMaximum - rMinimum) / 9
                    rValue <- rRampSPFmin
                }
                if (swhichparam == "Targ")
                {
                    rMinimum <- rtargetmin
                    rMaximum <- rtargetmax
                    rInterval <- (rMaximum - rMinimum) / 9
                    rValue <- rtargetmin
                }

                # create the ramped value file
                write(paste0('i,',swhichparam),file=paste0(sMarxanDir,"/",swhichparam,".csv"))
                write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)       
                for (i in 2:10)
                {
                  if (swhichparam == "Targ")
                  {
                      rValue <- rMinimum+((i-1)*rInterval)       # linear ramping for Target
                  } else {
                      rValue <- exp(rMinimum+((i-1)*rInterval))  # exponential ramping for BLM and SPF
                  }
                  write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)
                }
        
                # initialise a value summary file
                if (swhichparam == "BLM")
                {
                    write("i,BLM,cost,boundary length",file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"))
                }
                if (swhichparam == "SPF")
                {
                    write("i,SPF,cost,shortfall",file=paste0(sMarxanDir,"/output/output_SPFsummary.csv"))
                }
                if (swhichparam == "Targ")
                {
                    write('i,Targ,cost',file=paste0(sMarxanDir,"/output/output_Targsummary.csv"))
                }
                
                # load the ramped value file
                VALUEcsv <- read.csv(paste0(sMarxanDir,"/",swhichparam,".csv"))
                
                foreach(i=1:10) %dopar%   
                {
                    # copy files to execution directory
                    dir.create(paste0(sMarxanDir,"/core",i))
                    file.copy(paste0(sMarxanDir,"/MarOpt_v243_Mac64"),paste0(sMarxanDir,"/core",i,"/MarOpt_v243_Mac64"))
    
                    # read input.dat and edit parameters
                    inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                    iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
                    iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
                    iBLMparam <- which(regexpr("BLM",inputdat)==1)
                    iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                    iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                    iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
                    # read spec.dat
                    specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
                    if (swhichparam == "BLM")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",VALUEcsv[i,2])                      
                        specdat$spf <- ruserspf
                        specdat$prop <- rusertarg
                    }
                    if (swhichparam == "SPF")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
                        specdat$spf <- VALUEcsv[i,2]
                        specdat$prop <- rusertarg
                    }
                    if (swhichparam == "Targ")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
                        specdat$spf <- ruserspf
                        specdat$prop <- VALUEcsv[i,2]
                    }
                    # save spec.dat
                    write.csv(specdat,paste0(sMarxanDir,"/input/spec",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)
                    # edit parameters and save input.dat
                    inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
                    inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
                    inputdat[iSPECNAMEparam] <- paste0("SPECNAME spec",swhichparam,i,".dat")
                    inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",swhichparam,i)
                    inputdat[iNUMREPSparam] <- "NUMREPS 10"
                    writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input",swhichparam,i,".dat"))
                
                    # run Marxan
                    setwd(paste0(sMarxanDir,"/core",i))
                    cat(paste0(".Platform$pkgType ",.Platform$pkgType,"\n"))
                    if ((.Platform$pkgType == "mac.binary") || (.Platform$pkgType == "mac.binary.mavericks"))
                    {
                        cat("mac\n")
                        system(paste0("./MarOpt_v243_Mac64 -s input",swhichparam,i,".dat"))
                    } else {
                        cat("linux\n")
                        system(paste0("./MarOpt_v243_Linux64 -s input",swhichparam,i,".dat"))
                    }
                
                    # read the Marxan summary file
                    sumfile <- read.csv(paste0(sMarxanDir,"/output/output",swhichparam,i,"_sum.csv"))
  
                    # write to the value summary file
                    if (swhichparam == "BLM")
                    {
                        # write the cost and boundary length to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,5]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_BLMsummary",i,".csv"))
                    }
                    if (swhichparam == "SPF")
                    {
                        # write the cost and target shortfall to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv"))
                    }
                    if (swhichparam == "Targ")
                    {
                        # write the cost and target to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_Targsummary",i,".csv"))
                    }
                }     
                
                # compose summary table
                for (i in 1:10)
                {
                    if (swhichparam == "BLM")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_BLMsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"),append=TRUE)
                    }
                    if (swhichparam == "SPF")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_SPFsummary.csv"),append=TRUE)
                    }
                    if (swhichparam == "Targ")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_Targsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_Targsummary.csv"),append=TRUE)
                    }
                }

                # fetch the results
                PrepareDisplay()

                itestinput <<- itestinput + 1
                updateNumericInput(session, "testinput", value = itestinput)
            }
        }
    
        return(as.character(input$mrun))
    })

    outputmap <- reactive({
        cat("outputmap\n")
        
        input$testinput
        
        colourpalette <- c("white","green")
        tempputable <- sqldf("SELECT pu_id from pu_table")
        colnames(tempputable)[1] <- "PUID"
        
        # this is where we use swhichparam to select which parameter we are testing
        # and we use swhichrun to select between "Best", 1, 2, ..., 10
        # and we use iwhichmap to switch between the runs for different SPF values
        if (swhichrun == "best")
        {
            sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_best.csv")
        } else {
            if (swhichrun == "ssoln")
            {
                sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_ssoln.csv")
            } else {
        
            irun <- as.integer(swhichrun)
            sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_r",PadInt(irun),".csv")
            
            cat(paste0("\n run ",irun," filename ",sFilename," \n"))
            }
        }
        solution_table <- read.csv(sFilename)
        if (swhichrun == "ssoln")
        {
            cat("ssoln\n")
            
            colnames(solution_table)[1] <- "PUID"
            colnames(solution_table)[2] <- "SSOLN2"
            solution_table$SSOLN2 <- as.integer(solution_table$SSOLN2)
            values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
            values_ <- sqldf("SELECT SSOLN2 from values_") # + 1
            blueramp <- colorRampPalette(c("white","blue"))(16)
            colours <- rep(blueramp[1],nrow(values_))
            for (j in 1:nrow(values_))
            {
                if (pustatus[j] == 2)
                {
                    colours[j] <- "#40E0D0" # Turquoise
                } else {
                    if (pustatus[j] == 3)
                    {
                        colours[j] <- "grey"
                    } else {
                        colours[j] <- blueramp[round(15 / iNUMREPS * values_[j,])+1]
                    }
                }
            }
        } else {
            values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
            # plot the map
            values_ <- as.integer(unlist(sqldf("SELECT SOLUTION from values_") + 1))
            colours <- rep("white",each=length(values_))
            for (j in 1:length(values_))
            {
                if (pustatus[j] == 2)
                {
                    colours[j] <- "#40E0D0" # Turquoise
                } else {
                    if (pustatus[j] == 3)
                    {
                        colours[j] <- "grey"
                    } else {
                        colours[j] <- colourpalette[values_[j]]
                    }
                }
            }
            
        }
        plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
        addLines(puoutline,col="black")
    })

    outputplot <- reactive({
        cat("outputplot\n")

        input$testinput

        VALUEsummary <- read.csv(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv"))
        VALUElabel <- sqldf(paste0("SELECT ",swhichparam," from VALUEsummary"))
        colnames(VALUElabel)[1] <- "label"
        cat(paste0("VALUElabel ",VALUElabel$label,"\n"))
        if (swhichparam == "BLM")
        {
            colnames(VALUEsummary)[4] <- "boundary"
            VALUEsummary <- sqldf("SELECT cost, boundary from VALUEsummary")
        }
        if (swhichparam == "SPF")
        {
            colnames(VALUEsummary)[4] <- "shortfall"
            VALUEsummary <- sqldf("SELECT cost, shortfall from VALUEsummary")
        }
        if (swhichparam == "Targ")
        {
            colnames(VALUEsummary)[2] <- "target"
            VALUEsummary <- sqldf("SELECT cost, target from VALUEsummary")
        }
        colours <- rep("black",each=nrow(VALUEsummary))
        for (j in 1:nrow(VALUEsummary))
        {
            if (j == iwhichmap)
            {
                colours[j] <- "blue"
            }
        }
        plot(VALUEsummary,col=colours) 
        text(VALUEsummary,labels=VALUElabel$label,pos=4,col=colours)
    })

    outputtable <- reactive({
        cat("outputtable\n")

        input$testinput

        thetable <- read.csv(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv"),stringsAsFactors=FALSE)
        if (swhichparam == "BLM")
        {       
            colnames(thetable)[4] <- "boundary"
            thetable <- sqldf("SELECT BLM, cost, boundary from thetable")
            thetable$BLM <- as.character(thetable$BLM)
            iColumns <- 3
        }
        if (swhichparam == "SPF")
        {
            colnames(thetable)[4] <- "shortfall"
            thetable <- sqldf("SELECT SPF, cost, shortfall from thetable")
            thetable$SPF <- as.character(thetable$SPF)
            iColumns <- 3
        }
        if (swhichparam == "Targ")
        {
            colnames(thetable)[2] <- "target"
            thetable <- sqldf("SELECT target, cost from thetable")
            iColumns <- 2
        }
        for (i in (1:nrow(thetable)))
        {
            if (i == iwhichmap)
            {
                for (j in (1:iColumns))
                {
                    thetable[i,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,j],"</FONT>"))
                }
            }
        }
        return(thetable)
    })

    output$marxanmap <- renderPlot({
        print(outputmap())
    }, height=450,width=450)

    output$marxantable <- renderTable({
        dat <- data.frame(outputtable())
        dat
    }, sanitize.text.function = function(x) x)

    output$marxanplot <- renderPlot({
        print(outputplot())
    })

    output$buttonfeedback = renderText({
        runmarxan()
        sprintf("Finished")
    })
})

