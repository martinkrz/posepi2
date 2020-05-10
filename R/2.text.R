output$text2 = renderPrint({ 
  params1 = params2a()
  params2 = params2b()
  
  out   = df2()
  df1   = out[[1]]
  df2   = out[[2]]
  dfsir = out[[3]]
  
  table(title="SEIRS parameters 2",class="t2",rows=makerows(c(    
    "R0",varfmt(value=params2$R0,prec=1),
    "beta",varfmt(value=params2$beta,prec=3,units="/day"),
    "gamma",varfmt(value=params2$gamma,prec=3,units="/day"),
    "sigma",varfmt(value=params2$sigma,prec=3,units="/day"),
    "omega",varfmt(value=365*params2$omega,prec=3,units="/year"),
    "mu",varfmt(value=365*params2$mu,prec=3,units="/year"),
    "alpha",varfmt(value=params2$alpha,prec=3,units="/day"),
    "T_E",varfmt(value=params2$period/365,prec=2,units="years"),
    "Sinf",varfmt(value=params2$stars$S,prec=1,percent=1),
    "Einf",varfmt(value=params2$stars$E,prec=1,percent=1),
    "Iinf",varfmt(value=params2$stars$I,prec=1,percent=1),
    "Rinf",varfmt(value=params2$stars$R,prec=1,percent=1),
    "A",varfmt(value=params2$A/365,prec=1,units="years"),
    "CFR",varfmt(value=params2$cfr,prec=2,percent=1),
    "p",varfmt(value=params2$p,prec=0,percent=1),
    "pcrit",varfmt(value=params2$R0,prec=0,percent=1)
  )))
  table(title="SEIRS parameters 1",class="t1",rows=makerows(c(    
                                                     "R0",varfmt(value=params1$R0,prec=1),
                                                     "beta",varfmt(value=params1$beta,prec=3,units="/day"),
                                                     "gamma",varfmt(value=params1$gamma,prec=3,units="/day"),
                                                     "sigma",varfmt(value=params1$sigma,prec=3,units="/day"),
                                                     "omega",varfmt(value=365*params1$omega,prec=3,units="/year"),
                                                     "mu",varfmt(value=365*params1$mu,prec=3,units="/year"),
                                                     "alpha",varfmt(value=params1$alpha,prec=3,units="/day"),
                                                     "T_E",varfmt(value=params1$period/365,prec=2,units="years"),
                                                     "Sinf",varfmt(value=params1$stars$S,prec=1,percent=1),
                                                     "Einf",varfmt(value=params1$stars$E,prec=1,percent=1),
                                                     "Iinf",varfmt(value=params1$stars$I,prec=1,percent=1),
                                                     "Rinf",varfmt(value=params1$stars$R,prec=1,percent=1),
                                                     "A",varfmt(value=params1$A/365,prec=1,units="years"),
                                                     "CFR",varfmt(value=params1$cfr,prec=2,percent=1),
                                                     "p",varfmt(value=params1$p,prec=0,percent=1),
                                                     "pcrit",varfmt(value=params1$pc,prec=0,percent=1)
                                                     
  )))

  #cat(paste("<p>These interactive figures show how the SEIRS model of infection spread (see Equations tab) changes with varying parameters, such infectious period ",varfmt("1/gamma,"),"basic reproduction number",varfmt("R0,"),"latent period",varfmt("1/sigma,"),"immunity duration",varfmt("1/omega,"),"life expectancy",varfmt("1/mu"),"and vaccination level",varfmt("p."),sep=" "))
  cat(paste("<p>The phase plane is ...",varfmt("I(t)"),"vs.",varfmt("S(t)"),"..."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("The trajectories eventually reach endemic equilibrium and for",varfmt("R0"),"> 1 and when there is susceptibility recruitment through loss of immunity or birth, we have an 'open epidemic' and both",varfmt("Einf",params1$stars$E,prec=1,percent=1),"and",varfmt("Iinf",params1$stars$I,prec=1,percent=1),"are non-zero and",varfmt("Sinf"),"=",varfmt("1/R0.",params1$stars$S,prec=1,percent=1),"On the way to equilibrium, the trajectories exhibit epidemic waves whose period",varfmt("T_E",params1$period/365,units="years",prec=2),"(inter-epidemic interval) is a function of model parameters (see Equation tab)."))
  
})