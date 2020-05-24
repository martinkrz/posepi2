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
  
  cat(paste("<p>These interactive figures show how predictions from the SEIRS model (see Equations tab) change as a function of parameters such infectious period ",varfmt("(1/gamma),"),"basic reproduction number",varfmt("(R0),"),"latent period",varfmt("(1/sigma),"),"immunity duration",varfmt("(1/omega),"),"life expectancy",varfmt("(1/mu),"),"death onset",varfmt("(1/alpha)"),"and vaccination level",varfmt("p."),sep=" "))
  
  cat(paste("<h4>key observations</h4>"))
  cat(paste("When",varfmt("R0 > 1"),"and when there is susceptibility recruitment through loss of immunity",varfmt("(omega)"),"or birth",varfmt("(mu)"),"the epidemic is said to be 'open'. The SEIRS trajectories eventually reach endemic equilibrium, meaning that",varfmt("Einf",params$stars$E,prec=1,percent=1),"and",varfmt("Iinf",params$stars$I,prec=1,percent=1),"are non-zero."))
  cat(paste("<p>On the way to equilibrium, the trajectories exhibit epidemic waves whose period",varfmt("T_E",params$period/365,units="years",prec=2),"(inter-epidemic interval) is a function of model parameters. These waves should not be confused with seasonal epidemic trends, which are a result of temporal variation in",varfmt("beta.")))
  cat("<br clear=both>")

})

output$text1a = renderPrint({ 
  params = params1()
  cat(paste("<h4>Trajectories</h4>"))
})
output$text1b = renderPrint({ 
  params = params1()
  cat(paste("<h4>Phase plane</h4>"))
})