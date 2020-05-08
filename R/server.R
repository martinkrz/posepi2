server = function(input, output, session) {

  #Make sure that the mitigation R0final max is smaller than R0init
  observe(updateSliderInput(session, "R0final", max = input$R0init-R0_step))
  #Make sure that vaccination slider maxes out at p_c
  observe(updateSliderInput(session, "p1", max = floor(100*(1-1/input$R01))))

  # generate the SEIRS trajectories and phase plane
  df1 = reactive(calculate1(input$R01,input$ip1,input$lp1,input$id1,input$le1,input$p1/100)) %>% throttle(1000)
  p1  = reactive(    plots1(input$R01,input$ip1,input$lp1,input$id1,input$le1,input$p1/100)) %>% throttle(1000)
  
  #df2 = reactive(calculate2(input$R0init,input$R0final,input$ip2)) %>% throttle(1000)
  #df3 = reactive(calculate3(input$R0v,input$ip3)) %>% throttle(1000)
  
  # precompute all plots, indexes and titles for a panel, as needed
  #p2 = reactive(plots2(input$R0init,input$R0final,input$ip2)) %>% throttle(1000)
  #p3 = reactive(plots3(input$R0v,input$ip3)) %>% throttle(1000)
  
  report_timing = function(t0,t1) {
    if(do_timing) {
      print(sprintf("%.3f seconds",t1-t0))
    }
  }

  source("R/models.R",local=T)
  source("R/reactives.R",local=T)
  source("R/1.text.R",local=T)
  source("R/2.text.R",local=T)
  source("R/3.text.R",local=T)
  source("R/1.plot.R",local=T)
  source("R/2.plot.R",local=T)
  source("R/3.plot.R",local=T)
  source("R/titles.R",local=T)
  source("R/masthead.captions.R",local=T)
  
}