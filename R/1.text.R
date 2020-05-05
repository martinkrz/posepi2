output$text1 = renderPrint({ 
  ip    = input$ip1
  R0    = input$R01
  lp    = input$lp1
  id    = input$id1
  le    = input$le1
  vac   = input$vac/100
  gamma = 1/ip
  sigma = 1/lp
  omega = 1/(365*id)
  mu    = 1/(365*le)
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  imax  = (1-(1+log(R0))/(R0))
  df       = df1()
  rmax     = df[nrow(df),]$R
  smax     = df[nrow(df),]$S
  timaxidx = df$I == max(df$I)
  timax    = df[df$I == max(df$I),]$time
  stmax    = df[timaxidx,]$S
  rtmax    = df[timaxidx,]$R
  
  bgstr = sprintf(" (%s = %s%s = %.3f/day, %s = 1/%s = %.3f/day)",varfmt("beta"),varfmt("gamma"),varfmt("R0"),beta,tags$i("γ"),tags$i("ip"),gamma)
  
  table(title="SIR model parameters",rows=makerows(c("ip",varfmt(value=ip,units="days"),
                                                     "R0",varfmt(value=R0,prec=1),
                                                     "p",varfmt(value=vac,prec=0,percent=1),
                                                     "1/beta",varfmt(value=1/beta,prec=1,units="days"),
                                                     "1/gamma",varfmt(value=1/gamma,prec=0,units="days"),
                                                     "1/sigma",varfmt(value=1/sigma,prec=0,units="days"),
                                                     "1/omega",varfmt(value=1/(365*omega),prec=0,units="years"),
                                                     "1/mu",varfmt(value=1/(365*mu),prec=0,units="years"),
                                                     "pc",varfmt(value=1-1/R0,prec=0,percent=1),
                                                     "Imax",varfmt(value=imax,prec=1,percent=1),
                                                     "tmax",varfmt(value=timax,prec=1,units="days"),
                                                     "Stmax",varfmt(value=stmax,prec=1,percent=1),
                                                     "Rtmax",varfmt(value=rtmax,prec=1,percent=1),
                                                     "Rinf",varfmt(value=rmax,prec=1,percent=1),
                                                     "Sinf",varfmt(value=smax,prec=1,percent=1)
  )))
  
  cat(paste("<p><em>Except for the plot, text/table/caption still in progress.</em></p>"))
  cat(paste("<p>These interactive figures show how the SEIRS model of infection spread (see Equations tab) changes with varying parameters, such infectious period ",varfmt("ip,"),"basic reproduction number",varfmt("R0,"),"latent period",varfmt("1/sigma"),"immunity duration",varfmt("1/omega"),"life expectancy",varfmt("1/mu"),"and vaccination level",varfmt("p."),sep=" "))
  cat(paste("<p>We start with a look at the trajectories of each of the three groups (susceptible, exposed, infected, recovered) for an infectious period of",varfmt("ip",ip),"and a basic reproduction number",varfmt("R0",R0),bgstr,"with",varfmt("p",vac),"of the population vaccinated. For",varfmt("R0",R0),"the critical vaccination fraction for herd immunity is",varfmt("pc.",1-1/R0),"The initial susceptible group is the whole population minus the vaccinated fraction.",sep=" "))
  
  tmp   = df[df$I >= max(df$I)/2,]$time
  t1max = tmp[1]
  t2max = tmp[length(tmp)]
  
  cat(paste("<h4>key observations</h4>",sep=" "))
  cat(paste("<p>Peak infected fraction is",varfmt("Imax",imax),"and occurs at",varfmt("tmax",timax),"when",varfmt("Stmax",stmax),"and",varfmt("Rtmax.",rtmax),"The cumulative epidemic size at the end of the outbreak is",varfmt("Rinf,",rmax),"which means that",varfmt("Sinf",1-rmax),"of the population escapes infection.",sep=" "))
  cat(paste("<p>The infected fraction trajectory is not symmetric&mdash;it has a right skew. It takes",varfmt("t",timax-t1max),"to increase from the half-maximum to maximum (start of light orange area to vertical dotted line) but",varfmt("t",t2max-timax),"to decrease from maximum to half-maximum (vertical dotted line to end of light orange area). As",varfmt("R0"),"is decreased, this skew becomes less pronounced.",sep=" "))
  
})