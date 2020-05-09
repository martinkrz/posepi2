output$text1 = renderPrint({ 
  if(input$text1 == FALSE) {
    return(base::invisible())
  }
  ip    = input$ip1
  R0    = input$R01
  lp    = input$lp1
  id    = input$id1
  le    = input$le1
  alpha = 0
  p     = input$p1/100
  gamma = 1/ip
  sigma = 1/lp
  omega = 1/(365*id)
  mu    = 1/(365*le)
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  
  out   = df1()
  df    = out[[1]]
  dfsir = out[[2]]
  
  # in years
  A      = (omega+mu+gamma)/( (omega+mu) * (beta-gamma-mu) ) / 365
  stars  = stars(R0,gamma,sigma,omega,mu)
  period = ieperiod(R0,gamma,sigma,omega,mu) / 365

  table(title="SEIRS parameters",rows=makerows(c(    
                                                     #"R0",varfmt(value=R0,prec=1),
                                                     #"p",varfmt(value=p,prec=0,percent=1),
                                                     #"1/beta",varfmt(value=1/beta,prec=1,units="days"),
                                                     #"1/gamma",varfmt(value=1/gamma,prec=0,units="days"),
                                                     #"1/sigma",varfmt(value=1/sigma,prec=0,units="days"),
                                                     #"1/omega",varfmt(value=1/(365*omega),prec=0,units="years"),
                                                     #"1/mu",varfmt(value=1/(365*mu),prec=0,units="years")
                                                     "T_E",varfmt(value=period,prec=2,units="years"),
                                                     "Sinf",varfmt(value=stars$S,prec=1,percent=1),
                                                     "Einf",varfmt(value=stars$E,prec=1,percent=1),
                                                     "Iinf",varfmt(value=stars$I,prec=1,percent=1),
                                                     "Rinf",varfmt(value=stars$R,prec=1,percent=1),
                                                     "A",varfmt(value=A,prec=1,units="years"),
                                                     "pcrit",varfmt(value=1-1/R0,prec=0,percent=1)
  )))
  
  
  cat(paste("<p>These interactive figures show how the SEIRS model of infection spread (see Equations tab) changes with varying parameters, such infectious period ",varfmt("1/gamma,"),"basic reproduction number",varfmt("R0,"),"latent period",varfmt("1/sigma,"),"immunity duration",varfmt("1/omega,"),"life expectancy",varfmt("1/mu"),"and vaccination level",varfmt("p."),sep=" "))
  cat(paste("<p>Supplemental Figure 1 shows the trajectories of each of the groups in the model (susceptible, exposed, infected, recovered) for the model parameters shown in the table on the right. Supplemental Figure 2 shows the phase plot of",varfmt("I(t)"),"vs.",varfmt("S(t)"),"that corresponds to these trajectories."))
  cat(paste("<h4>key observations</h4>"))
  
  cat(paste("The trajectories eventually reach endemic equilibrium and for",varfmt("R0"),"> 1 and when there is susceptibility recruitment through loss of immunity or birth, we have an 'open epidemic' and both",varfmt("Einf",stars$E,prec=1,percent=1),"and",varfmt("Iinf",stars$I,prec=1,percent=1),"are non-zero and",varfmt("Sinf"),"=",varfmt("1/R0.",stars$S,prec=1,percent=1),"On the way to equilibrium, the trajectories exhibit epidemic waves whose period",varfmt("T_E",period,units="years",prec=2),"(inter-epidemic interval) is a function of model parameters (see Equation tab)."))
  
})