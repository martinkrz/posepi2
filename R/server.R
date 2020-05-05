server = function(input, output, session) {
  
  source("R/models.R",local=T)
  
  # Make sure that the mitigation R0final max is smaller than R0init
  observe(updateSliderInput(session, "R0final", max = input$R0init-R0_step))
  
  # generate the SIR trajectories and summaries for each panel, as needed
  df1 = reactive(calculate1(input$R01,input$ip1,input$lp1,input$id1,input$le1,input$vac/100)) %>% throttle(1000)
  #df2 = reactive(calculate2(input$R0init,input$R0final,input$ip2)) %>% throttle(1000)
  #df3 = reactive(calculate3(input$R0v,input$ip3)) %>% throttle(1000)
  
  # precompute all plots, indexes and titles for a panel, as needed
  p1 = reactive(plots1(input$R01,input$ip1,input$lp1,input$id1,input$le1,input$vac/100)) %>% throttle(1000)
  #p2 = reactive(plots2(input$R0init,input$R0final,input$ip2)) %>% throttle(1000)
  #p3 = reactive(plots3(input$R0v,input$ip3)) %>% throttle(1000)
  
  report_timing = function(t0,t1) {
    if(do_timing) {
      print(sprintf("%.3f seconds",t1-t0))
    }
  }
  
  output$masthead = renderPrint({ 
    cat(paste("<div id=natmeth><img src='img/nature.methods.png'/></div>",sep=""))
    cat(paste("<div id=mast><b>Supplemental Materials |</b> Bjornstad, O., Shea, K., Krzywinski, M. & Altman, N. Points of Significance: Modelling infectious epidemics II (2020) <i>Nat Meth</i> <b>17</b> (in press).</div>",sep=""))
  })
  
  sir_caption = function(tmax,vac) {
    paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",tmax),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 with",varfmt("S(0),",1-sir_init_i-vac,prec=3),varfmt("E(0)",sir_init_i),varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0).",vac,prec=3),sep=" ")
  }
  sir_caption_p = function(tmax) {
    paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",tmax),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 to",varfmt("S(0)",1-sir_init_i,prec=3)," &ndash; <i>p</i>,",varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0)")," = <i>p</i> for vaccination fraction",varfmt("p."),sep=" ")
  }
  
  source("R/reactives.R",local=T)
  source("R/1.text.R",local=T)
  source("R/2.text.R",local=T)
  source("R/3.text.R",local=T)
  source("R/1.plot.R",local=T)
  source("R/2.plot.R",local=T)
  source("R/3.plot.R",local=T)
  source("R/titles.R",local=T)
  
}