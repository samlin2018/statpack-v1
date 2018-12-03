#server.R

function(input, output, session) {

  #Panel 3
  output$t3_out1 <- renderText({
    paste("  =  ", 
          if (input$t3_ops1 == "+") {paste(input$t3_n1 + input$t3_n2)}
          else if (input$t3_ops1 == "-") {paste(input$t3_n1 - input$t3_n2)}
          else if (input$t3_ops1 == "x") {paste(input$t3_n1 * input$t3_n2)}
          else if (input$t3_ops1 == "/") {paste(input$t3_n1 / input$t3_n2)}
          else if (input$t3_ops1 == "^") {paste(input$t3_n1^input$t3_n2)}
          else if (input$t3_ops1 == "mod") {paste(input$t3_n1%%input$t3_n2)}
          else if (input$t3_ops1 == "sqrt") {paste(sqrt(input$t3_n1), "     \n || Note: only n1 is used")}
          else if (input$t3_ops1 == "log") {paste(log(input$t3_n1), "     \n || Note: only n1 is used")}
          else if (input$t3_ops1 == "log10") {paste(log10(input$t3_n1), "     \n || Note: only n1 is used")}
          else if (input$t3_ops1 == "e^") {paste(exp(input$t3_n1), "     \n || Note: only n1 is used")}
    )
  })
  
  ################
  
  #Panel 4 : Post Campaign Analysis
  ##4.1
  output$t4_out1 <- renderText({
    pvalue = {if (input$t4_c1 == "X") {prop.test(x=c(input$t4_x1, input$t4_x2), n=c(input$t4_n1, input$t4_n2))$p.value}
      else if (input$t4_c1 == "P") {prop.test(x=c(input$t4_n1 * input$t4_p1/100, input$t4_n2 * input$t4_p2/100), n=c(input$t4_n1, input$t4_n2))$p.value}}
    
    paste(if (pvalue <= 0.05) {" Significant to reject "} 
          else {" Failed (threshold = 0.05) to reject "}
    )
  })
  
  output$t4_out3 <- renderText({
    if (input$t4_c2 == T) {
      paste("Test Outcome: ")
    } else NULL
  })
  
  output$t4_out4 <- renderPrint({
    if (input$t4_c2 == TRUE) {
      res= {
        if (input$t4_c1 == "X") 
        {prop.test(x=c(input$t4_x1, input$t4_x2), n=c(input$t4_n1, input$t4_n2))}
        else 
        {prop.test(x=c(input$t4_n1 * input$t4_p1/100, input$t4_n2 * input$t4_p2/100), n=c(input$t4_n1, input$t4_n2))}
      }
    } else {
      res=NULL
    }
    print(res)
  })
  
  ##4.2
  output$t4_2_out1 <- renderText({
    pvalue = {
      if (input$t4_2_c1 == "X") {prop.test(x=as.numeric(input$t4_2_x), n=as.numeric(input$t4_2_n), p=as.numeric(input$t4_2_baseline)/100)$p.value}
      else if (input$t4_2_c1 == "P") {prop.test(x=input$t4_2_n * input$t4_2_p/100, n=as.numeric(input$t4_2_n), p=as.numeric(input$t4_2_baseline)/100)$p.value}
    }
    
    paste(if (pvalue <= 0.05) {" Significant to reject "} 
          else {" Failed to reject "}
    )
  })
  
  output$t4_2_out3 <- renderText({
    if (input$t4_2_c2 == T) {
      paste("Test Outcome: ")
    } else NULL
  })
  
  output$t4_2_out4 <- renderPrint({
    if (input$t4_2_c2 == T) {
      res={
        if (input$t4_2_c1 == "X") {
          prop.test(x=as.numeric(input$t4_2_x), n=as.numeric(input$t4_2_n), p=as.numeric(input$t4_2_baseline)/100)
        } else if (input$t4_2_c1 == "P") {
          prop.test(x=input$t4_2_n * input$t4_2_p/100, n=as.numeric(input$t4_2_n), p=as.numeric(input$t4_2_baseline)/100)
        }
      }
    } else {res=NULL}
    
    print(res)
  })
  ####################
  
  #Panel 5 : Pre Campaign Analysis
  output$t5_outmain1 <- renderText({paste(wordswitch(input$t5_tabs), 
                                          if (input$t5_tabs == "size" && input$t5_sampop != "eq") {wordswitch(input$t5_sampop)},
                                          " = ")
  })
  resultsOut <- reactive({
    results = tryCatch(
    if (input$t5_tabs == "size" & input$t5_sampop == "eq")
    {
      get2ptest(base_p=(input$t5_baseline)/100,
                expected_p={if (input$t5_tabs == "conv" ) {NULL} else {(input$t5_v5)/100}},
                n= {NULL} ,
                confidence={if (input$t5_tabs == "conf") {NULL} else {input$t5_v1}},
                power={if (input$t5_tabs == "pow") {NULL} else {input$t5_v2}})
    } else {
      get2p2ntest(base_p=(input$t5_baseline)/100,
                  expected_p={if (input$t5_tabs == "conv" ) {NULL} else {(input$t5_v5)/100}},
                  n1={if (input$t5_tabs == "size" & input$t5_sampop != "n1") {NULL} else {as.numeric(input$t5_v3)}},
                  n2={if (input$t5_tabs == "size" & input$t5_sampop != "n2") {NULL} else {as.numeric(input$t5_v4)}},
                  confidence={if (input$t5_tabs == "conf") {NULL} else {as.numeric(input$t5_v1)}},
                  power={if (input$t5_tabs == "pow") {NULL} else {as.numeric(input$t5_v2)}})
    }
    , error=function(err) -999 )
    res = paste(
      if (results== -999) {
        {if (input$t5_tabs == "conf") {1}
          else if (input$t5_tabs == "size") {paste("NA")}
          else if (input$t5_tabs == "conv") {paste("NA")}
          else {paste("NA")}}
      } else {
        {if (input$t5_tabs == "conf") {round(1-results, 5)}
          else if (input$t5_tabs == "size") {as.integer(results)}
          else if (input$t5_tabs == "conv") {paste(round(results*100,4),"%", sep="")}
          else {round(results,5)}}
      }
    )
    return(res)
  })
  output$t5_outmain2 <- renderText({
    res <- resultsOut()
    paste(res)
  })
  
  output$t5_warn1 <- renderText({
    res <- resultsOut()
    if (res== "NA" ) {
      paste("Note: no solution found. ",  
            if (input$t5_tabs == "size") 
              {paste("Please either increase sample size or change conversion rates.")} 
      )
    } else if (res==1 && input$t5_tabs == "conf") { paste("Sample Size is large. Confidence level is indefinitely close to 1.")}
  })
  
  output$t5_outmain3 <- renderText({
    ind1 = T
    ind2 = T
    ind3 = T
    ind4 = T
    ind5 = T
    
    if (input$t5_tabs == "conf") {ind1 = F} else
      if (input$t5_tabs == "pow") {ind2 = F} else
        if (input$t5_tabs == "conv" ) {ind5 = F} else
          if (input$t5_tabs == "size" && input$t5_sampop != "n1") {ind3 = F} else
            if (input$t5_tabs == "size" && input$t5_sampop != "n2") {ind4 = F}

    res <- resultsOut()
    paste("Given ",
          paste("Control Group response rate = ", input$t5_baseline,"%"),
          if (ind1 == T) {paste(", confidence level = ", input$t5_v1)} else {NULL},
          if (ind2 == T) {paste(", power = ", input$t5_v2)} else {NULL},
          if (ind3 == T) {paste(", sample size of control = ", input$t5_v3)} else {NULL},
          if (ind4 == T) {paste(", sample size of test = ", input$t5_v4)} else {NULL},
          if (ind5 == T) {paste(", expected test conversion rate = ", input$t5_v5,"%")} else {NULL},
          ", the estimate for ",
          paste(wordswitch(input$t5_tabs), if (input$t5_tabs == "size" && input$t5_sampop != "eq") {wordswitch(input$t5_sampop)}, "is equal to ", res)
    )
  })
  
  #RESET BUTTON
  observe({
    input$t5_ab1
    updateNumericInput(session, "t5_v1", value = 0.95)
    updateNumericInput(session, "t5_v2", value = 0.8)
  })
  
  ####################
  
  #Panel 6 : Animation
  
  output$t6_1_ui1 <- renderUI(sliderInput("t6_1_ani_1", label=h3("Simulated Sample Size"),
                                          min = 1000, max = 60000,
                                          value = 1000, step = 1000, sep = NULL,
                                          animate= animationOptions(interval = input$t6_1_3, loop = TRUE,
                                                                    playButton = icon("play-circle","fa-2x",lib="font-awesome"),
                                                                    pauseButton = icon("pause-circle","fa-2x",lib="font-awesome")))
  )
  
  samp_thr <- reactive(input$t6_1_ani_1) %>% throttle(1000)
  
  df_t6_1 <- reactive(
    {
      confidence_v <- seq(0,1, 0.05)
      test_rate <- input$t6_1_1
      control_rate <- input$t6_1_2
      sample <- samp_thr()
      power_v <- sapply(confidence_v, function(z) {
        get2ptest(base_p= (control_rate)/100,
                  expected_p= (test_rate)/100,
                  n= sample,
                  confidence=z,
                  power=NULL)
      })
      pconf_df = data.frame(cbind(conf=confidence_v,pow=power_v))
      return(pconf_df)
    }
  )
  
  output$t6_1_p1 <- renderPlot({
    df <- df_t6_1()
    ggplot(df, aes(x=conf, y=pow)) + xlim(0,1) +ylim(0,1) + geom_point(aes(size=2)) + 
      xlab("Confidence Level (0~1)") + ylab("Power Level (0~1)") +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size=14, face="bold")) +
      annotate("text", x=0.2, y=0.2, label=paste("Sample: ", samp_thr()), size=6)
  },
  bg = "transparent"
  )
  
  ####
  output$t6_2_ui1 <- renderUI(sliderInput("t6_2_ani_1", label=h3("Simulated Sample Size"),
                                          min = 1000, max = 60000,
                                          value = 1000, step = 1000, sep = NULL,
                                          animate= animationOptions(interval = input$t6_2_3, loop = TRUE,
                                                                    playButton = icon("play-circle","fa-2x",lib="font-awesome"),
                                                                    pauseButton = icon("pause-circle","fa-2x",lib="font-awesome")))
  )
  
  samp_thr2 <- reactive(input$t6_2_ani_1) %>% throttle(1000)
  
  df_t6_2 <- reactive(
    {
      control_rate <- as.numeric(input$t6_2_1)
      conf <- as.numeric(input$t6_2_2)
      power_v <- seq(1-conf+0.00001,1, 0.01)
      sample <- samp_thr2()
      test_conv_v <- sapply(power_v, function(z) {
        get2ptest(base_p= (control_rate)/100,
                  expected_p= NULL,
                  n= sample,
                  confidence=conf,
                  power=z) * 100
      })
      ptconv_df = data.frame(cbind(test_conv=test_conv_v,pow=power_v))
      return(ptconv_df)
    }
  )
  
  output$t6_2_p1 <- renderPlot({
    df <- df_t6_2()
    ggplot(df, aes(y=test_conv, x=pow)) + xlim(0,1) + ylim(as.numeric(input$t6_2_1), as.numeric(input$t6_2_1)+10 ) +
            geom_point(aes(size=2)) + xlab("Power Level (0 ~ 1)") + 
      ylab("Test: Minimal Detectable Conversion Rate (0% ~ 100%)") + 
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) +
      annotate("text", x=0.2, y=as.numeric(input$t6_2_1)+8, label=paste("Sample: ", samp_thr2()), size=6)
  },
  bg = "transparent"
  )
  
}


