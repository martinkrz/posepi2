server = function(input, output, session) {

  source("R/models.R",local=TRUE)
  source("R/reactives.R",local=TRUE)
  source("R/1.plot.R",local=TRUE)
  source("R/1.text.R",local=TRUE)
  source("R/2.text.R",local=TRUE)
  source("R/2.plot.R",local=TRUE)
  
  
  ### UI QOL settings
  # reset form
  observeEvent(input$refresh1,  { shinyjs::reset("form1") })
  observeEvent(input$text1,     { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy1", condition = input$text1) })
  observeEvent(input$captions1, { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption1", condition = input$captions1) })

  observeEvent(input$refresh2, { shinyjs::reset("form2") })
  observeEvent(input$text2,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy2", condition = input$text2) })
  observeEvent(input$captions2,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption2", condition = input$captions2) })
  
  #Make sure that vaccination slider maxes out at p_c
  #observe(updateSliderInput(session, "p1", max = floor(100*(1-1/input$R01))))

  # calculate parameters from all inputs
  params1  = reactive(get_params(input$R01,input$ip1,input$lp1, input$id1, input$le1,input$al1,input$p1/100)) %>% throttle(1000)
  params2a = reactive(get_params(input$R021,input$ip21,input$lp21,input$id21,input$le21,input$al21,input$p21/100)) %>% throttle(1000)
  params2b = reactive(get_params(input$R022,input$ip22,input$lp22,input$id22,input$le22,input$al22,input$p22/100)) %>% throttle(1000)
  #print(param1)
  # generate the SEIRS trajectories and phase plane
  #df1 = reactive(calculate1(reactive)) %>% throttle(1000)
  
  #df1 = reactive(calculate1(input$R01,input$ip1,input$lp1,input$id1,input$le1,input$al1,input$p1/100)) %>% throttle(1000)
  df1 = reactive(calculate1(params1())) %>% throttle(1000)
  p1  = reactive(    plots1(params1())) %>% throttle(1000)

  #df1 = isolate(calculate1(params1()))
  #p1  = isolate(plots1(params1()))
  
  df2 = reactive(calculate2(params2a(),params2b())) %>% throttle(1000)
  p2  = reactive(    plots2(params2a(),params2b())) %>% throttle(1000)
  
  report_timing = function(t0,t1) {
    if(do_timing) {
      print(sprintf("%.3f seconds",t1-t0))
    }
  }

  
  source("R/titles.R",local=TRUE)
  source("R/masthead.captions.R",local=TRUE)
  
}