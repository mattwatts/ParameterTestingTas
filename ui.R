# Author: Matt Watts
# Date: 10 Dec 2014
# Purpose: ParameterTestingTas web app ui.R

require(shiny)

shinyUI(pageWithSidebar(

    headerPanel("Tas Activity: Parameter Testing"),

    sidebarPanel(
        actionButton("mrun","Run"), 
        br(),
        br(),
        textOutput("buttonfeedback"),
        br(),
        selectInput("whichparam", "Parameter to test:",
                    choices = c("BLM Calibration","SPF Calibration","Target Sensitivity")),
        conditionalPanel(condition = "input.whichparam == 'SPF Calibration' | input.whichparam == 'Target Sensitivity'",
            numericInput("userblm", "BLM:",0,min=0)
        ),
        conditionalPanel(condition = "input.whichparam == 'BLM Calibration' | input.whichparam == 'Target Sensitivity'",
            numericInput("userspf", "SPF:",10,min=0)
        ),
        conditionalPanel(condition = "input.whichparam == 'BLM Calibration' | input.whichparam == 'SPF Calibration'",
            numericInput("usertarg", "Target:",0.1,min=0,max=1)
        ),
        conditionalPanel(condition = "input.whichparam == 'BLM Calibration'",
            numericInput("rampBLMmin", "BLM min:",0,min=0),
            numericInput("rampBLMmax", "BLM max:",10000000000000,min=0)
        ),
        conditionalPanel(condition = "input.whichparam == 'SPF Calibration'",
            numericInput("rampSPFmin", "SPF min:",0.0001,min=0),
            numericInput("rampSPFmax", "SPF max:",10000000000000,min=0)
        ),
        conditionalPanel(condition = "input.whichparam == 'Target Sensitivity'",
            numericInput("targetmin", "Target min:",0,min=0,max=1),
            numericInput("targetmax", "Target max:",1,min=0,max=1)
        ),
        br(),
        selectInput("whichmap", "Value to display:",
                    choices = c("1","2","3","4","5","6","7","8","9","10")),
        selectInput("whichrun", "Map to display:",
                    choices = c("Best Solution","Run 1","Run 2","Run 3","Run 4","Run 5",
                                "Run 6","Run 7","Run 8","Run 9","Run 10","Selection Frequency")),
        conditionalPanel(condition = "input.tabs == 'Map' & input.whichrun != 'Selection Frequency'",
                         HTML("<img src='http://marxan.net/images/green.png' /></a>"),
                         HTML("Selected"),
                         br(),
                         HTML("<img src='http://marxan.net/images/turquoise.png' /></a>"),
                         HTML("Existing Reserves"),
                         br(),
                         HTML("<img src='http://marxan.net/images/grey.png' /></a>"),
                         HTML("Excluded")
                        ),
        conditionalPanel(condition = "input.tabs == 'Map' & input.whichrun == 'Selection Frequency'",
                         HTML("Selection frequency"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue10.png' /></a>"),
                         HTML("10"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue9.png' /></a>"),
                         HTML("9"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue8.png' /></a>"),
                         HTML("8"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue7.png' /></a>"),
                         HTML("7"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue6.png' /></a>"),
                         HTML("6"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue5.png' /></a>"),
                         HTML("5"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue4.png' /></a>"),
                         HTML("4"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue3.png' /></a>"),
                         HTML("3"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue2.png' /></a>"),
                         HTML("2"),
                         br(),
                         HTML("<img src='http://marxan.net/images/blue1.png' /></a>"),
                         HTML("1"),
                         br(),
                         HTML("<img src='http://marxan.net/images/turquoise.png' /></a>"),
                         HTML("Existing Reserves"),
                         br(),
                         HTML("<img src='http://marxan.net/images/grey.png' /></a>"),
                         HTML("Excluded")
                        ),
        conditionalPanel(condition = "input.prop == -1",
                         numericInput("testinput", "Test Input", 0))

        ),

    mainPanel(
            tabsetPanel(id="tabs",
                tabPanel("Map",plotOutput('marxanmap'),
                               tableOutput('marxantable')),
                tabPanel("Plot",plotOutput('marxanplot'))
                       )
                 )
))

