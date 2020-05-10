output$text1intro = renderPrint({ 
  params = params1()
  
  #out   = df1()
  #df    = out[[1]]
  #dfsir = out[[2]]

  table(title="SEIRS parameters",rows=makerows(c(    
    "beta",varfmt(value=params$beta,prec=3,units="/day"),
    "gamma",varfmt(value=params$gamma,prec=3,units="/day"),
    "sigma",varfmt(value=params$sigma,prec=3,units="/day"),
    "omega",varfmt(value=365*params$omega,prec=3,units="/year"),
    "mu",varfmt(value=365*params$mu,prec=3,units="/year"),
    "alpha",varfmt(value=params$alpha,prec=3,units="/day"),
    "T_E",varfmt(value=params$period/365,prec=2,units="years"),
    "Sinf",varfmt(value=params$stars$S,prec=1,percent=1),
    "Einf",varfmt(value=params$stars$E,prec=1,percent=1),
    "Iinf",varfmt(value=params$stars$I,prec=1,percent=1),
    "Rinf",varfmt(value=params$stars$R,prec=1,percent=1),
    "A",varfmt(value=params$A/365,prec=1,units="years"),
    "CFR",varfmt(value=params$cfr,prec=2,percent=1),
    "pcrit",varfmt(value=params$pc,prec=0,percent=1)
  )))
  
  cat(paste("<p>These interactive figures show how the SEIRS model of infection spread (see Equations tab) changes with varying parameters, such infectious period ",varfmt("1/gamma,"),"basic reproduction number",varfmt("R0,"),"latent period",varfmt("1/sigma,"),"immunity duration",varfmt("1/omega,"),"life expectancy",varfmt("1/mu"),"and vaccination level",varfmt("p."),sep=" "))
  
  cat(paste("<h4>key observations</h4>"))
  cat(paste("The trajectories eventually reach endemic equilibrium and for",varfmt("R0"),"> 1 and when there is susceptibility recruitment through loss of immunity or birth, we have an 'open epidemic' and both",varfmt("Einf",params$stars$E,prec=1,percent=1),"and",varfmt("Iinf",params$stars$I,prec=1,percent=1),"are non-zero and",varfmt("Sinf"),"=",varfmt("1/R0.",params$stars$S,prec=1,percent=1),"On the way to equilibrium, the trajectories exhibit epidemic waves whose period",varfmt("T_E",params$period/365,units="years",prec=2),"(inter-epidemic interval) is a function of model parameters (see Equation tab)."))
  cat("<br clear=both>")

})

output$text1a = renderPrint({ 
  params = params1()
  cat(paste("<h4>Trajectories</h4>"))
  #cat(paste("<p>Figure 1 shows the trajectories of each of the groups in the model (susceptible, exposed, infected, recovered) for the model parameters shown in the table on the right. Figure 2 shows the phase plot of",varfmt("I(t)"),"vs.",varfmt("S(t)"),"that corresponds to these trajectories."))
})
output$text1b = renderPrint({ 
  params = params1()
  cat(paste("<h4>Phase plane</h4>"))
  #cat(paste("<p>Figure 1 shows the trajectories of each of the groups in the model (susceptible, exposed, infected, recovered) for the model parameters shown in the table on the right. Figure 2 shows the phase plot of",varfmt("I(t)"),"vs.",varfmt("S(t)"),"that corresponds to these trajectories."))
  
})