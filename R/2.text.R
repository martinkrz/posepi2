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

  cat(paste("<p>The phase plane of ",varfmt("I(t)"),"vs.",varfmt("S(t)"),"is helpful to understand how the endemic equilibria",varfmt("Iinf"),"and",varfmt("Sinf"),"change. When",varfmt("R0 > 1"),"and when there is susceptibility recruitment through loss of immunity",varfmt("(omega)"),"or birth",varfmt("(mu)"),"the epidemic is said to be 'open' and the phase plane trajectories will be spirals that converge to these equilibrium values."))
  cat(paste("<p>To see the rate at which the trajectory evolves, we can place points on the spiral that correspond to specific times from the start of the first epidemic (e.g. quarterly, yearly)."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("The endemic equilibrium",varfmt("Sinf = 1/R0"),"and is not significantly influenced by the latency paeriod",varfmt("1/sigma"),"when",varfmt("sigma ≫ mu."),"On the other hand, the",varfmt("Iinf"),"is influenced by",varfmt("omega"),"and decreases as the immunity duration",varfmt("1/omega"),"increases (see Equations tab)."))
  
})