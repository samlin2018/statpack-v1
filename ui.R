

fluidPage(
  theme= shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
                    #renderprint {
                      color: white;
                      background: blue;
                      font-family: 'Helvetica';
                      font-size: 20px;
                    }
                    "))
  )
  #,shinythemes::themeSelector()
  
  ,br()
  ,fluidRow(
    column(6, offset=0,   headerPanel(h1("StatPack", 
                                         style = "font-family: Courier;
                                         font-weight: 800; line-height: 1.1; 
                                         color: green;")))
    ,column(5, offset=0
            ,br()
            ,img(src="LB_logo.png", width=100, height=40, align="right")
            ,img(src="TheCore_logo.png", width=100, height=45, align="right"))
  )
  # ,br()
  # ,img(src="TheCore_logo.png", width=100, height=45, align="right"),
  # img(src="LB_logo.png", width=100, height=40, align="right"),
  # br(),
  # column(12, offset=0,   headerPanel(
  #   h1("StatPack", 
  #      style = "font-family: Courier;
  #      font-weight: 800; line-height: 1.1; 
  #      color: green;"))),
  ,hr()
  ,navlistPanel(
    id="nav_tabs",
    widths=c(3,9)
    
    ##############
    #,tabPanel("Basic Math", value=3,
    #          br(),
    #          fluidRow(column(width = 4, numericInput(inputId = "t3_n1", "n1 left", value=0)), 
    #                   column(width = 2, selectInput(inputId = "t3_ops1", "operators",
    #                                                 c("+","-","x","/","^","mod","sqrt","log","log10","e^"), selected = "+")),
    #                   column(width = 4, numericInput(inputId = "t3_n2", "n2 right", value=1.0))
    #          ),
    #          br(),
    #          fluidRow(column(width=10, 
    #                          h3(textOutput(outputId = "t3_out1"),
    #                             style="font-family: Helvetica; color: grey;",
    #                             align="center")
    #          )
    #          ),
    #          hr()
    # )
    
    #### Nav Tab - Pre-campaign Analysis ##############################
    ,tabPanel("Pre-campaign Analysis", value=5,
             tabsetPanel(id='t5_tabs',
                         # tabPanel("Reference", value='ref', br(),
                         #          h3("Power Analysis Relation Chart", 
                         #             style="font-weight: 400; font-height: 1.1; color: #004C99;"),
                         #          img(src="PA_relationchart2.png", width=600, height=270, align="center"),
                         #          br(), br(),
                         #          h5("Input h is a standardized effect size in statistical field;"),
                         #          h5("Given values of any three of (Confidence, Power, Sample Size, Expected), 
                         #             the rest will be calculated instantaneously;"),
                         #          hr()),
                         tabPanel("Conversion Rate", value = 'conv', br()),
                         tabPanel("Sample Size", value='size', br(), 
                                  radioButtons("t5_sampop", "What's known?", 
                                               c("Equal size of two groups" = "eq",
                                                 "Control group sample size is known" = "n1",
                                                 "Test group sample size is known" = "n2"))),
                         tabPanel("Confidence", value='conf', br()),
                         tabPanel("Power", value='pow', br())
                         ,conditionalPanel(
                             condition="input.t5_tabs == 'conv' | input.t5_tabs == 'size' | input.t5_tabs == 'conf' |
                                        input.t5_tabs == 'pow' & input.nav_tabs == 5 ",
                             sidebarLayout(
                               sidebarPanel(width = 4, 
                                            verticalLayout(
                                              h4("Inputs To Be Used"), br(),
                                              fluidRow(
                                                column(width = 12, numericInput(inputId = "t5_baseline", "Conversion Rate of Control Group: (0-100)%", value=2))
                                              )
                                              ,conditionalPanel(
                                                condition="input.t5_tabs != 'conv' & input.nav_tabs == 5",
                                                fluidRow(column(width = 12, numericInput(inputId = "t5_v5","Expected Conversion Rate of Test Group: (0-100)%", value=1)),
                                                         br())
                                              )
                                              ,conditionalPanel(
                                                condition="input.t5_tabs != 'size' | input.t5_sampop == 'n1' & input.nav_tabs == 5 ",
                                                fluidRow(column(width = 12, numericInput(inputId = "t5_v3","Sample Size of Control", value=1000)),
                                                         br())
                                              )
                                              ,conditionalPanel(
                                                condition="input.t5_tabs != 'size' | input.t5_sampop == 'n2' & input.nav_tabs == 5 ",
                                                fluidRow(column(width = 12, numericInput(inputId = "t5_v4","Sample Size of Test", value=1000)),
                                                         br())
                                              )
                                              ,conditionalPanel(
                                                condition="input.t5_tabs !='conf' & input.nav_tabs == 5",
                                                fluidRow(column(width = 12, numericInput(inputId = "t5_v1","Confidence", value=0.95)),
                                                         br())
                                              )
                                              ,conditionalPanel(
                                                condition="input.t5_tabs != 'pow' & input.nav_tabs == 5",
                                                fluidRow(column(width = 12, numericInput(inputId = "t5_v2","Power", value=0.8)),
                                                         br())
                                              
                                              )
                                              ,conditionalPanel(
                                                condition="input.t5_tabs == 'conv' | input.t5_tabs == 'size' & input.nav_tabs == 5 "
                                                ,fluidRow(column(width = 12, actionButton(inputId = "t5_ab1", "RESET")))
                                              )
                                              ,br()
                                            )
                               ),
                               mainPanel(#br(),
                                 fluidRow(
                                          column(width=10, offset=0.5,
                                                 h4("Estimates", 
                                                    style="font-weight: 400; font-height: 1.1; color: grey;"),
                                                 splitLayout(style="border: 1px solid silver;", 
                                                             cellWidths = c("65%","35%"),
                                                             h4(textOutput(outputId = "t5_outmain1"),
                                                                style="font-family: Helvetica; 
                                                                font-weight: 400; 
                                                                font-height: 1.1; 
                                                                color: #004C99;",
                                                                align="center"),
                                                             h4(textOutput(outputId = "t5_outmain2"),
                                                                style="font-weight: 400; font-height: 1.1; color: Orange;",
                                                                align="center"),
                                                             br()
                                                 ),
                                                 br(),
                                                 h5(textOutput(outputId = "t5_warn1"),
                                                    style="font-family: Helvetica; color: red"),
                                                 br(),
                                                 verticalLayout(
                                                   h4("Use Case Description", 
                                                      style="font-family: Helvetica;
                                                      font-weight: 400;
                                                      font-height: 1.1;
                                                      color: grey;"
                                                   ),
                                                   h5(textOutput(outputId = "t5_outmain3"),
                                                      style="font-family: Helvetica;"
                                                   )
                                                 )
                                          )
                                 )  #fluidRow close  
                               )  #mainpanel close
                             ), #sidebarlayout close
                             hr()
                           )  #conditionalPanel close
             )
    )
    
    #### Nav Tab - Post-campaign Analysis ############################# 
    , tabPanel("Post-campaign Analysis", value=4,
             tabsetPanel(id = "t4_tabs",
                         tabPanel("Two-Proportions Z-test", value = 4.1,
                                  br(),
                                  h3("Two-Proportions Z-test Analysis",
                                     style="font-weight: 400; font-height:1.1; color: #004C99"),
                                  br(),
                                  verticalLayout(
                                    fluidRow(
                                      column(width = 4, numericInput(inputId="t4_n1", "sample size of test", value=10000)),
                                      column(width = 4, numericInput(inputId="t4_n2", "sample size of control", value=20000))
                                    ),
                                    radioButtons(inputId = "t4_c1","Which input?",
                                                 c("Response by Counts" = "X",
                                                   "Response by Percentage" = "P"),
                                                 width = "100%"
                                    )
                                    ,conditionalPanel(condition="input.t4_c1 == 'X' & input.nav_tabs == 4 & input.t4_tabs == 4.1",
                                                      numericInput(inputId="t4_x1", "counts of response in test", value=5000),
                                                      numericInput(inputId="t4_x2", "counts of response in control", value=11000)
                                    )
                                    ,conditionalPanel(condition="input.t4_c1 == 'P' & input.nav_tabs == 4 & input.t4_tabs == 4.1",
                                                      numericInput(inputId="t4_p1", "percent of response in test: [0,100]%", value=50),
                                                      numericInput(inputId="t4_p2", "percent of response in control: [0,100]%", value=55)
                                    )
                                  )
                                  ,hr()
                                  ,h5("Test the null hypothesis: the response of test and control has no difference.")
                                  ,fluidRow(
                                    column(width=4, h4(textOutput(outputId="t4_out1"), style="color: darkred", align="right")),
                                    column(width=4, h4(" null hypothesis. ", align="left"))
                                  )
                                  ,br()
                                  ,checkboxInput(inputId="t4_c2", "Details of Test Results", value=FALSE)
                                  # ,conditionalPanel(condition="input.nav_tabs == 4 & input.t4_tabs == 4.1 & input.t4_c2 == TRUE",
                                  #                  h5(textOutput(outputId="t4_out3")),
                                  #                  verbatimTextOutput(outputId="t4_out4")
                                  # )
                                  ,h5(textOutput(outputId="t4_out3"))
                                  ,verbatimTextOutput(outputId="t4_out4")
                                  ,br()

                         )
                         ,tabPanel("One-Sample Z-test", value=4.2, br()
                                   ,h3("One-Sample Z-test Analysis",
                                      style="font-weight: 400; font-height:1.1; color: #004C99")
                                   ,br()
                                   ,verticalLayout(
                                     fluidRow(
                                       column(width = 4, numericInput(inputId="t4_2_baseline", "Proportion of Baseline [0,100]%", value=50 ))
                                       ,column(width = 4, numericInput(inputId="t4_2_n", "sample size of control", value=20000))
                                     )
                                     ,radioButtons(inputId = "t4_2_c1","Which input?",
                                                  c("Response by Counts" = "X",
                                                    "Response by Percentage" = "P"),
                                                  width = "100%"
                                     )
                                     ,conditionalPanel(condition="input.t4_2_c1 == 'X' & input.nav_tabs == 4 & input.t4_tabs == 4.2",
                                                       numericInput(inputId="t4_2_x", "counts of response in sample", value=10500)
                                     )
                                     ,conditionalPanel(condition="input.t4_2_c1 == 'P' & input.nav_tabs == 4 & input.t4_tabs == 4.2",
                                                       numericInput(inputId="t4_2_p", "percent of response in sample [0,100]%", value=52.5)
                                     )
                                   )
                                   ,hr()
                                   ,h5("Test the null hypothesis: the response rate of test sample has no difference against known baseline response rate.")
                                   ,fluidRow(
                                     column(width=4, h4(textOutput(outputId="t4_2_out1"), style="color: darkred", align="right")),
                                     column(width=4, h4(" null hypothesis. ", align="left"))
                                   )
                                   ,br()
                                   ,checkboxInput(inputId="t4_2_c2", "Details of Test Results", value=FALSE)
                                   ,h5(textOutput(outputId="t4_2_out3"))
                                   ,verbatimTextOutput(outputId="t4_2_out4")
                                   ,br()
                                   
                         )
             )
    )
    , tabPanel("Animation Plot", value=6, 
            tabsetPanel(id="t6_tabs",
                        tabPanel("Power - Confidence", value=6.1, br()
                                 ,h3("Power vs Confidence Level by Sample Size (equal), given Conversion difference",
                                       style="font-weight: 400; font-height:1.1; color: #004C99")
                                 ,br(),
                                 sidebarLayout(
                                   sidebarPanel(width=4
                                                ,sliderInput("t6_1_1","Test: conversion rate %", 
                                                             min=0, max=100, value=52, step=0.5)
                                                ,sliderInput("t6_1_2","Control: conversion rate %", 
                                                             min=0, max=100, value=50, step=0.5)
                                                ,sliderInput("t6_1_3","Animation Speed (millisecond)",
                                                             min=500, max=5000, value=1000, step=500)
                                                ,uiOutput("t6_1_ui1")
                                   ),
                                   mainPanel(width=7
                                             ,plotOutput("t6_1_p1", height=600, width=800)
                                   )
                                 )
                                 
                        ),
                        tabPanel("Power - Conversion", value=6.2, br()
                                 ,h3("Power vs Minimal Detectable Conversion Rate of Test by Sample Size",
                                     style="font-weight: 400; font-height:1.1; color: #004C99")
                                 ,br()
                                 ,sidebarLayout(
                                   sidebarPanel(width=4
                                                ,sliderInput("t6_2_1", "Control: conversion rate %",
                                                             min=0, max=100, value=50, step=0.5)
                                                ,sliderInput("t6_2_2", "Confidence Level",
                                                             min=0, max=1, value=0.95, step=0.05)
                                                ,sliderInput("t6_2_3", "Animation Speed (millisecond)",
                                                             min=500, max=5000, value=1000, step=500)
                                                ,uiOutput("t6_2_ui1")
                                   ),
                                   mainPanel(width=7
                                             ,plotOutput("t6_2_p1", height=600, width=800)
                                     
                                   )
                                 )
                          
                        )
            )
    )
    
    ,tabPanel("Reference", value=2
              # ,h3("Statistical Terms",
              #    style="font-weight: 400; font-height:1.1; color: grey")
              # ,br()
              ,navbarPage("Statistical Terms", id="nav_tabs_t2" #widths = c(3,9), well = T
                            , tabPanel("Confidence Interval"
                                       , value = 2.1
                                       , br()
                                       , p(tags$strong("The confidence interval")
                                           , " is the plus-or-minus figure usually reported in newspaper or television opinion poll results. "
                                           , "For example, if you use a confidence interval of 4 and 47% percent of your sample picks an answer 
                                              you can be “sure” that if you had asked the question
                                              of the entire relevant population between 43% (47-4) and 51% (47+4) would have picked that answer...."
                                            , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , helpText(a("Confidence Interval in Wiki"
                                                    ,target="_blank"
                                                    ,href="https://en.wikipedia.org/wiki/Confidence_interval"))
                                       , helpText(a("Confidence Interval by Yale"
                                                    ,target="_blank"
                                                    ,href="http://www.stat.yale.edu/Courses/1997-98/101/confint.htm")))
                            , tabPanel("Confidence Level"
                                       , value = 2.2
                                       , br()
                                       , p(tags$strong("The confidence level")
                                           , " tells you how sure you can be. 
                                            It is expressed as a percentage and represents how often 
                                            the true percentage of the population who would pick an answer lies within the confidence interval. 
                                            The 95% confidence level means you can be 95% certain; 
                                            the 99% confidence level means you can be 99% certain. 
                                            Most researchers use the 95% confidence level....."
                                           , tags$code("Range: [0,1]")
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , helpText(a("Confidence Interval & Confidence Intervals by UConn"
                                                      ,target="_blank"
                                                      ,href="https://researchbasics.education.uconn.edu/confidence-intervals-and-levels/")))
                            , tabPanel("H0 & H1 - Null and Alternative Hypothesis"
                                       , value = 2.3
                                       , br()
                                       , p("The statement being tested in a test of statistical significance
                                           is called the", tags$strong("null hypothesis"),
                                           ". The test of significance is designed to assess the strength of the evidence against the null hypothesis. 
                                           Usually, the null hypothesis is a statement of 'no effect' or 'no difference'."
                                           , em("by Moore, David; McCabe, George (2003). Introduction to the Practice of Statistics (4 ed.). 
                                                New York: W.H. Freeman and Co. p. 438. ISBN 9780716796572.")
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded") 
                                       , helpText(a("Null Hypothesis by Wiki"
                                                    , target="_blank"
                                                    , href="https://en.wikipedia.org/wiki/Null_hypothesis")))
                            , tabPanel("P value"
                                       , value = 2.4
                                       , br()
                                       , p(tags$strong("P-value")
                                           , " if in a campaign test with two proportions of conversions, is the probability  
                                           that the mean difference of two offer conversion rates is equally or more extreme than what we observed, like 2%, 
                                           assuming our Null Hypothesis is true. If the null hypothese is true, given p-value = 0.01,
                                           it means it's very rare to see x% difference but we observed it. 
                                           Hence it's a significant sign that null hypothesis could be false."
                                           , tags$code("Range: [0,1]")
                                           ,  style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , helpText(a("P-value by Wiki"
                                                    , target="_blank"
                                                    , href="https://en.wikipedia.org/wiki/P-value")) )
                            , tabPanel("Power Analysis"
                                       , value = 2.5
                                       , br()
                                       , p(tags$strong("Power Analysis")
                                           , " is commonly used in designing an experiment, to address three quesions: 
                                           (1) How large a sample is sufficient to make a reliable/accurate statistical judgment.
                                           (2) How likely your test will be to detect effects. 
                                           (3) How the size of effects will be returned, given other conditions."
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , br()
                                       , p("It includes four elements: Sample Size/Sizes, Significance Level (alpha or 1-Confidence Level), 
                                           Power, Effect Size (Cohen's h). Given any three of the four elements, the rest will be calculated."
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , br()
                                       , helpText(a("Power Analysis reference"
                                                    , target="_blank"
                                                    , href="http://www.statsoft.com/textbook/power-analysis"))
                                       , helpText(a("Cohen's h in Wiki"
                                                    , target="_blank"
                                                    , href="https://en.wikipedia.org/wiki/Cohen%27s_h"))
                                       , img(src="PA_relationchart2.png", width=600, height=270, align="center"))
                            , tabPanel("Power value"
                                       , value = 2.6
                                       , br()
                                       , p(tags$strong("Power")
                                           , " is the probability of correctly rejecting a known false null hypothesis. 
                                           In the context of binary classification, the power of a test is also called True Positive Rate."
                                          , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , helpText(a("Power in Wiki"
                                                    , target="_blank"
                                                    , href="https://en.wikipedia.org/wiki/Power_(statistics)")))
                            , tabPanel("Significance Level"
                                       , value = 2.7
                                       , br()
                                       , p(tags$strong("Significance Level")
                                           , " also called 'alpha', is more of a threshold to determine 
                                           if the p-value of the result, 
                                           as the probability of obtaining a result at least as extreme given H0 were true,
                                           indicates enough significance. 
                                           The result is statistically significant when p-value < alpha ."
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , helpText(a("Statistical Significance"
                                                    , target="_blank"
                                                    , href="https://en.wikipedia.org/wiki/Statistical_significance")))
                            , tabPanel("Proportion Test"
                                       , value = 2.8
                                       , br()
                                       , p(tags$strong("One-Sample tests")
                                           , " is used when a sample is being compared to the population from a hypothesis. 
                                           For example, given a conversion rate 5% and a set of responses from a sample, 
                                           we can use one-sample test to evaluate if the response rate is 
                                           significantly higher or lower than the 5% baseline."
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , br()
                                       , p(tags$strong("Two-Sample tests")
                                           , " are used to evaluate the difference on performance
                                           when two campaigns/samples were designed and executed. 
                                           It's knowned as a test and control experiment."
                                           , style="font-family: Times; font-weight: 300; font-size: 18px; font-stretch: expanded")
                                       , helpText(a("Test Statistics"
                                                    , target="_blank"
                                                    , href="https://en.wikipedia.org/wiki/Test_statistic")) )
              )
              , br()
                            
    )
    
  ),
  
  img(src="LB_logo2b.png", width=300, height=75, align="right"),
  br(), br(), br()
)
