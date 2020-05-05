output$text3 = renderPrint({ 
  R0       = input$R0v
  ip       = input$ip3
  gamma    = 1/ip
  pc_crit  = 1-1/R0
  pc_range = seq(0,pc_crit,by=0.1)
  
  table(title="SIR model parameters",makerows(c("ip",varfmt(value=ip,units="days"),
                                                "R0",varfmt(value=R0,prec=1),
                                                "beta",varfmt(value=gamma*R0,prec=4,units="/day"),
                                                "gamma",varfmt(value=gamma,prec=4,units="/day"),
                                                "pc",varfmt(value=1-1/R0,prec=0,percent=1)
  )))
  
  
  cat(paste("<p>When the population is vaccinated, some of the susceptibles are moved directly into the recovered group.</p>",sep=" "))
  cat(paste("<p>Shown are infected and recovered trajectories as vaccination level is increased from",varfmt("p",0),"to",varfmt("p",pc_range[length(pc_range)]),"for an epidemic with ",varfmt("R0.",R0),sep=" "))
  cat(paste("<h4>Key observations</h4>"))
  cat(paste("<p>If the population if vaccinated at the critical fraction",varfmt("pc",1-1/R0),"an oubreak does not occur. At this level of vaccination the population has so-called 'herd immunity'.", sep=" "))
})
