server = function(input, output, session) {
  
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
  
  # estimate how long the outbreak lasts - this will be used as time axis max
  sir_t_bound = function(beta,gamma,vac=0,tol=sir_init_i/2,tmax=1000,step=1) {
    # get a quick solution out to long time
    R0 = beta/gamma
    if(R0 <= 1 | vac >= 1-1/R0) {
      return(1000)
    }
    df     = sir(beta,gamma,vac,tmax=tmax,step=step)
    df_cut = df[df$I >= tol,]
    return( df_cut[nrow(df_cut),]$time )
  }
  # estimate how long the outbreak lasts - this will be used as time axis max
  seirs_t_bound = function(R0,gamma,sigma,omega,mu,vac=0,tol=sir_init_i/2,tmax=1000,step=1) {
    # get a quick solution out to long time
    beta  = beta(R0,gamma,sigma,mu,model="seirs")
    if(R0 <= 1 | vac >= 1-1/R0) {
      return(1000)
    }
    df     = seirs(beta,gamma,sigma,omega,mu,vac,tmax=tmax,step=step)
    df_cut = df[df$I >= tol,]
    return(df_cut[nrow(df_cut),]$time)
  }
  
  seirs_system=function(t,x,params) {
    S=x[1]
    E=x[2]
    I=x[3]
    R=x[4]
  
    mu   = params["mu"]
    beta = params["beta"]
    omega= params["omega"]
    sigma= params["sigma"]
    gamma= params["gamma"]

    dS = -beta*S*I + omega*R  + mu     - mu*S 
    dE = -sigma*E  + beta*S*I          - mu*E
    dI = -gamma*I  + sigma*E           - mu*I
    dR = -omega*R  + gamma*I           - mu*R
    #     ^          ^          ^      ^
    #     outflow    inflow     birth  death
    res= c(dS, dE, dI, dR)
    list(res)
  }
  
  sir_system=function(t, x, parms){
    S = x[1]
    I = x[2]
    R = x[3]
    beta  = parms["beta"]
    mu    = parms["mu"]    # we're not using this here
    gamma = parms["gamma"]
    N     = parms["N"]
    dS    = mu*(N-S)  - beta*S*I/N
    dI    = beta*S*I/N - I*(mu + gamma)
    dR    = gamma*I - mu*R
    res   = c(dS,dI,dR)
    list(res)
  }
  
  # vac is the vaccination fraction - it is removed from S but not added to R
  seirs = function(R0=3, gamma=1/14, sigma=1/7, omega=365/5, mu=365/76, vac=0, tmax=20, step=sir_system_time_step) {
    beta  = beta(R0,gamma,sigma,mu,model="seirs")
    time  = seq(0, tmax, by=step)
    parms = c(mu=mu, N=1, beta=beta, gamma=gamma, sigma=sigma, omega=omega, mu=mu)
    start = c(S=1-sir_init_i-vac, E=sir_init_i, I=0, R=0)
    out   = ode(y     = start, 
                times = time, 
                func  = seirs_system, 
                parms = parms)
    out   = cbind(out,data.frame(R0=beta/gamma,vac=vac))
    return(as.data.frame(out))
  }
  
  # beta for SEIRS (default) and anything else (e.g. SIR) 
  beta = function(R0,gamma,sigma=0,mu=0,model="seirs") {
    if(model == "seirs") {
      f = (gamma + mu) * (sigma + mu) / sigma
    } else {
      f = gamma
    }
    beta = R0 * f
    return(beta)
  }
  
  # vac is the vaccination fraction - it is removed from S but not added to R
  sir = function(beta=3, gamma=2, vac=0, tmax=20, step=sir_system_time_step) {
    time  = seq(0, tmax, by=step)
    parms = c(mu=0, N=1, beta=beta, gamma=gamma)
    start = c(S=1-sir_init_i-vac, I=sir_init_i, R=0)
    out   = ode(y     = start, 
                times = time, 
                func  = sir_system, 
                parms = parms)
    out   = cbind(out,data.frame(R0=beta/gamma,vac=vac))
    return(as.data.frame(out))
  }
  
  report_timing = function(t0,t1) {
    if(do_timing) {
      print(sprintf("%.3f seconds",t1-t0))
    }
  }
  
  # Panel 1: trajectories for a given R0, ip and vac
  calculate1 = function(R0,ip,lp,id,le,vac) {
    gamma = 1/ip
    sigma = 1/lp
    omega = 1/(id*365)
    mu    = 1/(le*365)
    beta  = beta(R0,gamma,sigma,mu,model="seirs")
    tmax  = seirs_t_bound(beta,gamma,sigma,omega,mu,vac)
    out   = seirs(R0=R0, gamma=gamma, sigma=sigma, omega=omega, mu=mu, vac=vac, tmax=tmax, step=tmax/sir_system_steps)
    return(out)
  }
  
  # Panel 2: step over R0 from Rinit to R0final
  calculate2 = function(R0init,R0final,ip) {
    R0step   = 0.1 * ((R0final>R0init)*2-1)
    R0_range = seq(R0init,R0final,length.out=5)
    summary  = data.frame()
    gamma    = 1/ip
    vac      = 0
    t0 = Sys.time()
    tmax     = sir_t_bound(gamma*min(R0_range),gamma,vac=vac)
    trajectories = data.frame()
    for (R0 in R0_range) {
      beta  = gamma * R0
      df    = sir(beta = beta, gamma = gamma, vac = vac, tmax=tmax, step = tmax/sir_system_steps)
      trajectories = rbind(trajectories,df)
      imax  = df[df$I == max(df$I),]$I
      rmax  = df[nrow(df),]$R
      smin  = df[nrow(df),]$S
      timax = df[df$I == max(df$I),]$time
      summary = rbind(summary,data.frame(R0=R0,timax=timax,smin=smin,imax=imax,rmax=rmax))
    }
    t1 = Sys.time()
    report_timing(t0,t1)
    return(list(trajectories,summary))
  }
  
  # Panel 3: step 0 to p_c for a given R0
  calculate3  = function(R0,ip) {
    gamma     = 1/ip
    beta      = gamma*R0
    pc_crit   = 1-1/R0
    vac_range = seq(0,pc_crit-0.001,by=0.1)
    t0 = Sys.time()
    tmax      = sir_t_bound(gamma*R0,gamma,max(vac_range))
    summary   = data.frame()
    trajectories = data.frame()
    for (vac in vac_range) {
      df    = sir(beta = beta, gamma = gamma, vac = vac, tmax = tmax, step = tmax/sir_system_steps)
      trajectories = rbind(trajectories,df)
      imax  = df[df$I == max(df$I),]$I
      rmax  = df[nrow(df),]$R
      smin  = df[nrow(df),]$S
      timax = df[df$I == max(df$I),]$time
      summary = rbind(summary,data.frame(R0=R0,vac=vac,timax=timax,smin=smin,imax=imax,rmax=rmax))
    }
    t1 = Sys.time()
    report_timing(t0,t1)
    return(list(trajectories,summary))
  }
  
  output$masthead = renderPrint({ 
    cat(paste("<div id=natmeth><img src='img/nature.methods.png'/></div>",sep=""))
    cat(paste("<div id=mast><b>Supplemental Materials |</b> Bjornstad, O., Shea, K., Krzywinski, M. & Altman, N. Points of Significance: Modelling infectious epidemics II (2020) <i>Nat Meth</i> <b>17</b> (in press).</div>",sep=""))
    
  })
  
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
  
  output$text2 = renderPrint({ 
    R0init  = input$R0init
    R0final = input$R0final
    capacity= input$capacity/100
    ip      = input$ip2
    gamma   = 1/ip
    df      = df2()
    summary = df[[2]]
    trajectories = df[[1]]
    
    imaxhi   = max(summary$imax)
    imaxlo   = min(summary$imax)
    timaxmin = min(summary$timax)
    timaxmax = max(summary$timax)
    
    R0min    = min(trajectories$R0)
    R0max    = max(trajectories$R0)
    
    # high R0
    rmaxhi   = max(summary$rmax)
    sminhi   = min(summary$smin)
    
    # low R0
    rmaxlo   = min(summary$rmax)
    sminlo   = max(summary$smin)
    
    # trajectories for R0max and R0min (m suffix)
    ibeyondcap  = trajectories[trajectories$I > capacity & trajectories$R0 == R0max,]
    ibeyondcapm = trajectories[trajectories$I > capacity & trajectories$R0 == R0min,]
    
    # times beyond capacity
    t0beyondcap  = min(ibeyondcap$time)
    t1beyondcap  = max(ibeyondcap$time)
    t0beyondcapm = min(ibeyondcapm$time)
    t1beyondcapm = max(ibeyondcapm$time)
    
    sir_system_time_step = max(trajectories$time)/sir_system_steps
    #print(sir_system_time_step)
    # areas of the curve beyond capacity
    sum           = sir_system_time_step  * sum(trajectories[trajectories$R0 == R0max,]$I)
    sumbeyondcap  = sir_system_time_step  * sum(ibeyondcap$I - capacity)
    sumincap  = sir_system_time_step  * sum(ibeyondcap$I)
    summ          = sir_system_time_step * sum(trajectories[trajectories$R0 == R0min,]$I)
    sumbeyondcapm = sir_system_time_step * sum(ibeyondcapm$I - capacity)
    sumincapm = sir_system_time_step * sum(ibeyondcapm$I)
    
    bgstrR0min = sprintf(" (%s = %s%s%s = %.3f, %s = 1/%s = %.3f)",tags$i("β"),tags$i("γ"),tags$i("R"),tags$sub(0),gamma*R0min,tags$i("γ"),tags$i("ip"),gamma)
    bgstrR0max = sprintf(" (%s = %s%s%s = %.3f, %s = 1/%s = %.3f)",tags$i("β"),tags$i("γ"),tags$i("R"),tags$sub(0),gamma*R0max,tags$i("γ"),tags$i("ip"),gamma)
    
    table(title="mitigated model",makerows(c("ip",varfmt(value=ip,units="days"),
                                       "R0",varfmt(value=R0final,prec=1),
                                       "beta",varfmt(value=gamma*R0final,prec=4,units="/day"),
                                       "gamma",varfmt(value=gamma,prec=4,units="/day"),
                                       "Imax",varfmt(value=imaxlo,prec=1,percent=1),
                                       "tmax",varfmt(value=timaxmax,prec=1,units="days"),
                                       "Rinf",varfmt(value=rmaxlo,prec=1,percent=1),
                                       "Sinf",varfmt(value=sminlo,prec=1,percent=1),
                                       "C",varfmt(value=capacity,percent=1,prec=0),
                                       "t1",varfmt(value=t0beyondcap,units="days"),
                                       "t2",varfmt(value=t1beyondcap,units="days"),
                                       "deltat",varfmt(value=t1beyondcap-t0beyondcap,units="days"),
                                       "Ideltat",varfmt(value=sumincapm/summ,percent=1,prec=1,units="cases"),
                                       "I>deltat",varfmt(value=sumbeyondcapm/summ,percent=1,prec=1,units="cases")
    )))
    table(title="original model",makerows(c("ip",varfmt(value=ip,units="days"),
                                      "R0",varfmt(value=R0init,prec=1),
                                      "beta",varfmt(value=gamma*R0init,prec=4,units="/day"),
                                      "gamma",varfmt(value=gamma,prec=4,units="/day"),
                                      "Imax",varfmt(value=imaxhi,prec=1,percent=1),
                                      "tmax",varfmt(value=timaxmin,prec=1,units="days"),
                                      "Rinf",varfmt(value=rmaxhi,prec=1,percent=1),
                                      "Sinf",varfmt(value=sminhi,prec=1,percent=1),
                                      "C",varfmt(value=capacity,percent=1,prec=0),
                                      "t1",varfmt(value=t0beyondcapm,units="days"),
                                      "t2",varfmt(value=t1beyondcapm,units="days"),
                                      "deltat",varfmt(value=t1beyondcapm-t0beyondcapm,units="days"),
                                      "Ideltat",varfmt(value=sumincap/sum,percent=1,prec=1,units="cases"),
                                      "I>deltat",varfmt(value=sumbeyondcap/sum,percent=1,prec=1,units="cases")
    )))
    
    cat(paste("<p>If we can mitigate the",varfmt("R0"),"of an epidemic, we gain valuable public health benefits. This decrease can be achieved with strategies such as social distancing (decreases number of contacts) or hygiene (decreases chance of infection on contact). The goal of mitigation is to reduce and delay the peak infected fraction. This is the so-called 'flattening the curve'."))
    
    cat(paste("<p>For example, by lowering",varfmt("R0",R0init),"to",varfmt("R0,",R0final),"the peak infection is lowered from ",varfmt("Imax",imaxhi),"to",varfmt("Imax,",imaxlo),"which is a ",sprintf("%.1f%%",100*(imaxhi-imaxlo)/imaxhi),"relative decrease. Also, peak time is shifted from",varfmt("tmax",timaxmin),"to",varfmt("tmax,",timaxmax),"which is a relative delay of",sprintf("%.1f%%.",100*(timaxmax-timaxmin)/timaxmin),"The cumulative epidemic size drops from",varfmt("Rinf",rmaxhi),"to",varfmt("Rinf",rmaxlo),"which is a relative decrease of",sprintf("%.1f%%.",100*(rmaxhi-rmaxlo)/rmaxhi),sep=" "))
    
    cat(paste("<h4>key observations</h4>",sep=" "))
    
    if(! nrow(ibeyondcap) & ! nrow(ibeyondcapm)) {
      cat(paste("<p>All curves are below hospital capacity.",sep=""))
    }
    if(nrow(ibeyondcap)) {
      cat(paste("<p>If we assume a hospital capacity of",sprintf("%.0f%%,",100*capacity),"the original curve for",varfmt("R0",R0init),"would be beyond capacity from",varfmt("t1",t0beyondcap),"to",varfmt("t2",t1beyondcap),"for a duration of",varfmt("deltat.",t1beyondcap-t0beyondcap),"During this time we would see",varfmt("Ideltat",value=sumincap/sum),"cases with",varfmt("I>deltat",value=sumbeyondcap/sum),"of all cases falling above capacity. ",sep=" "))
    }
    if(nrow(ibeyondcapm)) {
      cat(paste("<p>The mitigated curve for ",varfmt("R0",R0final),"would be beyond capacity from ",varfmt("t1",t0beyondcapm),"to",varfmt("t2",t1beyondcapm),"for a duration of",varfmt("deltat.",t1beyondcapm-t0beyondcapm),"During this time we would see",varfmt("Ideltat",value=sumincapm/summ),"cases with",varfmt("I>deltat",sumbeyondcapm/summ),"of all cases falling above capacity.",sep=" "))
      
      s0 = sumbeyondcap/sum
      s1 = sumbeyondcapm/summ
      
      cat(paste("<p>In this scenario, mitigation efforts have reduced the area of the curve above capacity from",varfmt("I>deltat",s0),"to",varfmt("I>deltat",s1),"which is a relative decrease of",sprintf("%.1f%%.",100*(s0-s1)/s0),sep=" "))      
      
      cat(paste("<p>In reality, for large populations the ",tags$a(href="https://en.wikipedia.org/wiki/List_of_countries_by_hospital_beds","hospital capacity is less than 1% with over 60% occupancy"),".",sep=""))
    }
  })
  
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
  
  sir_caption = function(tmax,vac) {
    paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",tmax),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 with",varfmt("S(0),",1-sir_init_i-vac,prec=3),varfmt("E(0)",sir_init_i),varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0).",vac,prec=3),sep=" ")
  }
  sir_caption_p = function(tmax) {
    paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",tmax),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 to",varfmt("S(0)",1-sir_init_i,prec=3)," &ndash; <i>p</i>,",varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0)")," = <i>p</i> for vaccination fraction",varfmt("p."),sep=" ")
  }
  
  plots1 = function(R0,ip,lp,id,le,vac) {
    df    = df1()
    plot  = ggplot(data=df,aes(x=time))
    imax  = df[df$I == max(df$I),]$I
    rmax  = df[nrow(df),]$R
    timax = df[df$I == max(df$I),]$time
    tmax  = max(df$time)
    
    tmp    = df[df$I >= max(df$I)/2,]
    t1max  = tmp[1,]$time
    it1max = tmp[1,]$I
    t2max  = tmp[nrow(tmp),]$time
    it2max = tmp[nrow(tmp),]$I
    
    #plot = plot + geom_area(data=tmp,aes(y=I),fill=as.list(palette)$I,alpha=0.2,size=plot_line_width)
    plot = plot + geom_ribbon(data=tmp,aes(x=time,ymin=0.0001,ymax=I),fill=as.list(palette)$I,alpha=0.2,size=plot_line_width)
    
    plot = plot + geom_line(aes(y=S,colour="S"),size=plot_line_width)
    plot = plot + geom_line(aes(y=E,colour="E"),size=plot_line_width)
    plot = plot + geom_line(aes(y=R,colour="R"),size=plot_line_width)
    plot = plot + geom_line(aes(y=I,colour="I"),size=plot_line_width)
    plot = plot + geom_hline(yintercept=imax,colour=as.list(palette)$I,size=plot_line_width/2,linetype="dashed")
    plot = plot + geom_hline(yintercept=rmax,colour=as.list(palette)$R,size=plot_line_width/2,linetype="dashed")
    plot = plot + geom_vline(xintercept=timax,colour=as.list(palette)$I,size=plot_line_width/2,linetype="dotted")
    
    #plot = plot + geom_line(data=data.frame(x=c(t1max,t1max),y=c(0,it1max)),aes(x,y),colour=as.list(palette)$I,size=plot_line_width/2,linetype="solid")
    #plot = plot + geom_line(data=data.frame(x=c(t2max,t2max),y=c(0,it2max)),aes(x,y),colour=as.list(palette)$I,size=plot_line_width/2,linetype="solid")
    
    title = sprintf("Infection spread for <i>R</i><sub>0</sub> = %.1f and <i>ip</i> = %d days with %.0f%% vaccination.",R0,ip,100*vac)
    plot = plot + my.plot_legend + my.plot_axis(xmin = 0,xmax = tmax,ymin=0,ymax=1,log10=input$log1 )
    
    caption = paste("The SEIRS model trajectories for susceptible, infected and recovered groups for",varfmt("R0",R0),"and",varfmt("ip",ip),"with",varfmt("p",vac),"vaccination. Lines indicate peak infected fraction",varfmt("Imax",imax),"(orange dashed line), the time at which peak infected fraction occurs",varfmt("tmax",tmax),"(dotted orange line) and the cumulative epidemic size",varfmt("Rinf",rmax),"(blue dashed line).",sir_caption(tmax,vac),sep=" ")
    
    return(list(list(plot_theme(plot)),list(1),list(title),list(caption)))
  }
  
  plots2 = function(R0init,R0final,ip) {
    gamma        = 1/ip
    capacity     = input$capacity/100
    plotI        = ggplot()
    plotR        = ggplot()
    plotImax     = ggplot()
    plotRmax     = ggplot()
    plotImaxt    = ggplot()
    plotRmaxt    = ggplot()
    df           = df2()
    trajectories = df[[1]]
    summary      = df[[2]]
    tmax         = max(trajectories[1]$time)
    R0_range     = sort(unique(trajectories$R0),decreasing=TRUE)
    for (R0 in R0_range) {
      line_width = plot_line_width
      is_intermediate = R0 < R0init & R0 > R0final
      if(input$show2) {
        if(is_intermediate) {
          line_width = 1
        }
      } else if (is_intermediate) {
        next
      }
      beta  = gamma*R0
      df    = trajectories[trajectories$R0 == R0,]
      if(R0 == min(R0_range) || R0 == max(R0_range)) {
        alpha = 1
        fill  = 1
        if(R0 == min(R0_range)) {
          fillc  = as.list(palette)$G
        } else {
          fillc  = as.list(palette)$I
        }
      } else {
        alpha = 0.5
        fill  = 0
      }
      plotI = plotI + geom_line(data=df,aes(x=time,y=I,colour="I"),alpha=alpha,size=line_width)
      plotR = plotR + geom_line(data=df,aes(x=time,y=R,colour="R"),alpha=alpha,size=line_width)  
      if(fill) {
        #plotI = plotI + geom_area(data=df,aes(x=time,y=I),fill=fillc,alpha=0.1)
      }
    }
    for(R0 in c(R0init,R0final)) {    
      plotR = plotR + annotate(geom="label",x=input$endtime2*0.95,
                               y=summary[summary$R0 == R0,]$rmax,hjust="right",
                               label=sprintf("%.2f",R0),
                               fill="white",color="black")
    }
    
    plotI = plotI + geom_hline(yintercept=min(summary$imax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dashed")
    plotI = plotI + geom_hline(yintercept=max(summary$imax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dashed") 
    plotI = plotI + geom_hline(yintercept=capacity,
                               linetype="solid",colour=as.list(palette)$C,size=plot_line_width/2) 
    
    plotI = plotI + geom_vline(xintercept=min(summary$timax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dotted")
    plotI = plotI + geom_vline(xintercept=max(summary$timax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dotted")
    
    plotR = plotR + geom_hline(yintercept=capacity,
                               linetype="solid",colour=as.list(palette)$C,size=plot_line_width/2) 
    plotR = plotR + geom_hline(yintercept=min(summary$imax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dashed")
    plotR = plotR + geom_hline(yintercept=max(summary$imax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dashed")
    
    plotR = plotR + geom_hline(yintercept=min(summary$rmax),
                               colour=as.list(palette)$R,size=plot_line_width/2,linetype="dashed")
    plotR = plotR + geom_hline(yintercept=max(summary$rmax),
                               colour=as.list(palette)$R,size=plot_line_width/2,linetype="dashed")
    
    plotR = plotR + geom_vline(xintercept=min(summary$timax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dotted")
    plotR = plotR + geom_vline(xintercept=max(summary$timax),
                               colour=as.list(palette)$I,size=plot_line_width/2,linetype="dotted")
    
    plotImax = plotImax + geom_line(data=summary,aes(x=R0,y=imax,colour="I"),size=plot_line_width) + 
      geom_point(data=summary,aes(x=R0,y=imax,colour="I"),size=3)
    plotRmax = plotRmax + geom_line(data=summary,aes(x=R0,y=rmax,colour="R"),size=plot_line_width) + 
      geom_point(data=summary,aes(x=R0,y=rmax,colour="R"),size=3)
    plotImaxt = plotImaxt + geom_line(data=summary,aes(x=timax,y=imax,colour="I"),size=plot_line_width) + 
      geom_point(data=summary,aes(x=timax,y=imax,colour="I"),size=3)
    plotRmaxt = plotRmaxt + geom_line(data=summary,aes(x=timax,y=rmax,colour="R"),size=plot_line_width) + 
      geom_point(data=summary,aes(x=timax,y=rmax,colour="R"),size=3)
    
    plotImax = plotImax + geom_hline(yintercept=capacity,
                                     linetype="solid",colour=as.list(palette)$C,size=plot_line_width/2) 
    plotImaxt = plotImaxt + geom_hline(yintercept=capacity,
                                       linetype="solid",colour=as.list(palette)$C,size=plot_line_width/2) 
    
    plotI = plotI + annotate(geom="label",x=tmax,y=capacity,hjust="right",label="hospital capacity", fill="black",color="white")
    plotR = plotR + annotate(geom="label",x=tmax,y=capacity,hjust="right",label="hospital capacity", fill="black",color="white")
    plotImax = plotImax + annotate(geom="label",x=R0init,y=capacity,hjust="right",label="hospital capacity", fill="black",color="white")
    plotImaxt = plotImaxt + annotate(geom="label",x=max(summary$timax),y=capacity,hjust="right",label="hospital capacity", fill="black",color="white")
    
    plotImaxt = plotImaxt + geom_label(data=summary,
                                       aes(x=timax,y=imax,label=R0),
                                       nudge_y=-0.02,
                                       size=5,
                                       fill=NA,
                                       label.size=NA,
                                       hjust="center")
    plotRmaxt = plotRmaxt + geom_label(data=summary,
                                       aes(x=timax,y=rmax,label=R0),
                                       nudge_y=-0.01,
                                       size=5,
                                       fill=NA,
                                       label.size=NA,
                                       hjust="center")
    
    
    plotI    = plotI + my.plot_legend
    plotR    = plotR + my.plot_legend
    plotImax = plotImax + my.plot_legend
    plotRmax = plotRmax + my.plot_legend
    plotImaxt = plotImaxt + my.plot_legend
    plotRmaxt = plotRmaxt + my.plot_legend
    
    plotI = plotI + my.plot_axis(xmin = 0,xmax = tmax, ymax = max(summary$imax))
    plotR = plotR + my.plot_axis(xmin = 0,xmax = tmax, ymax = max(summary$rmax))
    
    plotImax = plotImax + my.plot_axis(xlab="R0",
                                       ylab="peak infected (%)",
                                       #xmax=tmax,
                                       ymax=max(summary$imax))
    plotRmax = plotRmax + my.plot_axis(xlab="R0",
                                       ylab="cumulative epidemic size (%)",
                                       #xmax=tmax,
                                       ymin=min(summary$rmax),
                                       ymax=max(summary$rmax))
    plotImaxt = plotImaxt + my.plot_axis(xlab="infection peak time (days)",
                                         ylab="peak infected (%)",
                                         #xmax=tmax,
                                         ymax=max(summary$imax))
    plotRmaxt = plotRmaxt + my.plot_axis(xlab="peak infection time (days)",
                                         ylab="cumulative epidemic size (%)",
                                         #xmax=tmax,
                                         ymin=min(summary$rmax),
                                         ymax=max(summary$rmax))
    
    
    # trajectories for R0max and R0min (m suffix)
    ibeyondcap  = trajectories[trajectories$I > capacity & trajectories$R0 == R0init,]
    ibeyondcapm = trajectories[trajectories$I > capacity & trajectories$R0 == R0final,]
    
    # times beyond capacity
    t0beyondcap = min(ibeyondcap$time)
    t1beyondcap = max(ibeyondcap$time)
    t0beyondcapm = min(ibeyondcapm$time)
    t1beyondcapm = max(ibeyondcapm$time)
    
    tmp   = df[df$I >= max(df$I)/2,]
    t1max = tmp[1,]$time
    it1max = tmp[1,]$I
    t2max = tmp[nrow(tmp),]$time
    it2max = tmp[nrow(tmp),]$I
    
    if(nrow(ibeyondcap)) {
      #plotI = plotI + geom_area(data=ibeyondcap,aes(x=time,y=I),fill=as.list(palette)$I,alpha=0.2,size=plot_line_width)
      plotI = plotI + geom_ribbon(data=ibeyondcap,aes(x=time,ymin=capacity,ymax=I),fill=as.list(palette)$I,alpha=0.5)
    }
    if(nrow(ibeyondcapm)) {
      #plotI = plotI + geom_area(data=ibeyondcapm,aes(x=time,y=I),fill=as.list(palette)$C,alpha=0.2,size=plot_line_width)
      plotI = plotI + geom_ribbon(data=ibeyondcapm,aes(x=time,ymin=capacity,ymax=I),fill=as.list(palette)$I,alpha=0.2)
      
    }
    
    if(nrow(ibeyondcap)) {
      #plotI = plotI + geom_rect(data=data.frame(t0=t0beyondcap,t1=t1beyondcap,y0=0,y1=max(ibeyondcap$I)),
      #                          mapping=aes(xmin=t0,xmax=t1,ymin=y0,ymax=y1),fill=as.list(palette)$I,alpha=0.1)
    }
    if(nrow(ibeyondcapm)) {
      #plotI = plotI + geom_rect(data=data.frame(t0=t0beyondcapm,t1=t1beyondcapm,y0=0,y1=max(ibeyondcapm$I)),
      #                          mapping=aes(xmin=t0,xmax=t1,ymin=y0,ymax=y1),fill=as.list(palette)$C,alpha=0.1)
    }
    
    imaxhi   = max(summary$imax)
    imaxlo   = min(summary$imax)
    timaxmin = min(summary$timax)
    timaxmax = max(summary$timax)
    
    label1 = sprintf('paste(italic(R)[0]," = %.1f")',R0init)
    label2 = sprintf('paste(italic(R)[0]," = %.1f")',R0final)
    
    xy1 = trajectories[trajectories$R0 == R0init & trajectories$I <= 0.9*imaxhi & trajectories$time > timaxmin,]
    xy2 = trajectories[trajectories$R0 == R0final & trajectories$I <= 0.9*imaxlo & trajectories$time > timaxmax,]
    
    plotI = plotI + annotate(geom="label",label.size=NA,size=5,
                             x=xy1[1,]$time + timaxmin*0.1,y=xy1[1,]$I,
                             hjust="left",vjust="top",label=label1,fill="white",parse=TRUE)
    plotI = plotI + annotate(geom="label",label.size=NA,size=5,
                             x=xy2[1,]$time + timaxmax*0.1,y=xy2[1,]$I,
                             hjust="left",vjust="top",label=label2,fill="white",parse=TRUE)
    
    rmaxhi   = max(summary$rmax)
    rmaxlo   = min(summary$rmax)
    
    plotR = plotR + annotate(geom="label",label.size=NA,size=5,
                             x=tmax,y=rmaxhi,
                             hjust="right",vjust="middle",label=label1,fill="white",parse=TRUE)
    plotR = plotR + annotate(geom="label",label.size=NA,size=5,
                             x=tmax,y=rmaxlo,
                             hjust="right",vjust="middle",label=label2,fill="white",parse=TRUE)
    
    plotlist = list(plot_theme(plotI),plot_theme(plotR),
                    plot_theme(plotImax),plot_theme(plotRmax),
                    plot_theme(plotImaxt),plot_theme(plotRmaxt))
    
    subtitle = sprintf("for <i>R</i><sub>0</sub> = %.1f to %.1f with <i>ip</i> = %d days.",R0init,R0final,ip)
    indexes  = list(2,3,4,5,6,7)
    titles   = list(sprintf("Infected trajectory %s",subtitle),
                    sprintf("Recovered trajectory %s",subtitle),
                    sprintf("Peak infected fraction %s",subtitle),
                    sprintf("Cumulative epidemic size %s",subtitle),
                    sprintf("Peak infected fraction %s",subtitle),
                    sprintf("Cumulative epidemic size %s",subtitle))
    
    subcap = ""
    if(input$show2) {
      str    = sprintf("%s and %s.",paste(R0_range[2:length(R0_range)-2],collapse=", "),R0_range[length(R0_range)-1])
      subcap = paste("Also shown are curves for intermediate values of ",varfmt("R0")," = ",str,collapse="")
    }
    captions = list(
      paste("The infected fraction for",varfmt("R0",R0init),"and",varfmt("R0.",R0final),subcap,
            "This outbreak mitigation lowers the peak infected fraction (horizontal dashed lines) from",varfmt("Imax",imaxhi),"to",varfmt("Imax",imaxlo),"and delays its onset (vertical dotted line) from",varfmt("t",timaxmin),"to",varfmt("t.",timaxmax),sir_caption(tmax,0),collapse=" "),
      paste("The recovered fraction for",varfmt("R0",R0init),"and",varfmt("R0.",R0final),subcap,
            "This outbreak mitigation lowers the cumulative epidemic size (horizontal blue dashed lines) from",varfmt("Rinf",rmaxhi),"to",varfmt("Rinf.",rmaxlo),"Orange grid lines are same as those in above plot.",sir_caption(tmax,0),collapse=" "),
      paste("The decrease in the peak infected fraction",varfmt("Imax"),"for epidemic mitigation from",varfmt("R0",R0init),"to",varfmt("R0.",R0final),"Note that",varfmt("Imax"),"drops faster than linearly.",sir_caption(tmax,0),collapse=" "),
      paste("The decrease in cumulative epidemic size",varfmt("Rinf"),"for epidemic mitigation from",varfmt("R0",R0init),"to",varfmt("R0.",R0final),"Note that",varfmt("Rinf"),"drops faster than linearly.",sir_caption(tmax,0),collapse=" "),
      paste("The relationship between the peak infected fraction",varfmt("Imax"),"and the time of its onset",varfmt("tmax"),"for epidemic mitigation from",varfmt("R0",R0init),"to",varfmt("R0.",R0final),"Note that the delay in",varfmt("tmax"),"increases faster than linearly.",sir_caption(tmax,0),collapse=" "),
      paste("The relationship between the cumulative epidemic size",varfmt("Rinf"),"and the time of the peak infection",varfmt("tmax"),"for epidemic mitigation from",varfmt("R0",R0init),"to",varfmt("R0.",R0final),"Note that the plot is not linear.",sir_caption(tmax,0),collapse=" ")
      
    )
    
    return(list(plotlist,indexes,titles,captions))
  }
  
  plots3 = function(R0,ip) {
    gamma    = 1/ip
    beta     = gamma*R0
    vac_crit = 1-1/R0
    plotI    = ggplot()
    plotR    = ggplot()
    plotImax = ggplot()
    plotRmax = ggplot()
    df           = df3()
    trajectories = df[[1]]
    summary      = df[[2]]
    tmax         = max(trajectories[1]$time)
    vac_range     = sort(unique(trajectories$vac),decreasing=FALSE)
    for (vac in vac_range) {
      df    = trajectories[trajectories$vac == vac,]
      summarythis = summary[summary$vac == vac,]
      plotI = plotI + geom_line(data=df,aes(x=time,y=I,colour="I"),size=1.5)
      plotR = plotR + geom_line(data=df,aes(x=time,y=R,colour="R"),size=1.5)
    }
    
    plotI = plotI + annotate(geom="label",x=summary$timax,y=summary$imax,hjust="center",
                             label=sprintf("%s%%",100*summary$vac), fill="white",color="black")
    plotR = plotR + annotate(geom="label",x=tmax*0.95,y=summary$rmax,hjust="right",
                             label=sprintf("%s%%",100*summary$vac), fill="white",color="black")
    
    plotImax = plotImax + geom_line(data=summary,aes(x=vac,y=imax,colour="I"),size=1.5) + 
      geom_point(data=summary,aes(x=vac,y=imax,colour="I"),size=3)
    plotRmax = plotRmax + geom_line(data=summary,aes(x=vac,y=rmax,colour="R"),size=1.5) + 
      geom_point(data=summary,aes(x=vac,y=rmax,colour="R"),size=3)
    
    plotI    = plotI + my.plot_legend
    plotR    = plotR + my.plot_legend
    plotImax = plotImax + my.plot_legend
    plotRmax = plotRmax + my.plot_legend
    
    plotI = plotI + my.plot_axis(xmin=0,xmax=tmax,ymax = max(summary$imax))
    plotR = plotR + my.plot_axis(xmin=0,xmax=tmax,ymax = max(summary$rmax))
    plotImax = plotImax + my.plot_axis_both_percent(xlab="vaccination (%)",ylab="peak infected (%)",
                                                    ymax = max(summary$imax))
    plotRmax = plotRmax + my.plot_axis_both_percent(xlab="vaccination (%)",ylab="cumulative epidemic size (%)",
                                                    ymax = max(summary$rmax))
    
    #plotI = plotI + scale_x_continuous(lim=c(0,tmax))
    #plotR = plotR + scale_x_continuous(lim=c(0,tmax))
    
    subtitle = sprintf("for <i>R</i><sub>0</sub> = %.1f with <i>ip</i> = %d days with vaccination level %.0f%% to %.0f%% (<i>p</i><sub>c</sub> = %.1f%%)",
                       R0,ip,100*min(vac_range),100*max(vac_range),100*(1-1/R0))
    
    plots = list(plot_theme(plotI),plot_theme(plotR),plot_theme(plotImax),plot_theme(plotRmax))
    indexes = list(8,9,10,11)
    titles = list(sprintf("Infected trajectory %s.",subtitle),
                  sprintf("Recovered trajectory %s.",subtitle),
                  sprintf("Peak infected fraction %s.",subtitle),
                  sprintf("Cumulative epidemic size %s.",subtitle))
    
    str    = sprintf("%s and %s.",paste(vac_range[2:length(vac_range)-2],collapse=", "),vac_range[length(vac_range)-1])
    #subcap = paste("Also shown are curves for intermediate values of ",varfmt("R0")," = ",str,collapse="")
    
    captions = list(
      paste("The infected fraction for",varfmt("R0",R0),"and vaccination fraction from",varfmt("p",vac_range[1]),"to",varfmt("p",vac_range[length(vac_range)]),"shown by labels on plots. If the population is vaccinated at or above the critical fraction",varfmt("pc",1-1/R0),"then an oubreak will not occur.",sir_caption_p(tmax),collapse=" "),
      paste("The cumulative epidemic size for",varfmt("R0",R0),"and vaccination fraction from",varfmt("p",vac_range[1]),"to",varfmt("p",vac_range[length(vac_range)]),"shown by labels on plots. If the population is vaccinated at or above the critical fraction",varfmt("pc",1-1/R0),"then an oubreak will not occur.",sir_caption_p(tmax),collapse=" "),
      paste("The relationship between the peak infected fraction",varfmt("Imax"),"and the vaccination fraction from",varfmt("p",vac_range[1]),"to",varfmt("p",vac_range[length(vac_range)]),"for an epidemic with",varfmt("R0.",R0),"If the population is vaccinated at or above the critical fraction",varfmt("pc",1-1/R0),"then an oubreak will not occur.",sir_caption_p(tmax),collapse=" "),
      paste("The relationship between the cumulative epidemic size",varfmt("Rinf"),"and the vaccination fraction from",varfmt("p",vac_range[1]),"to",varfmt("p",vac_range[length(vac_range)]),"for an epidemic with",varfmt("R0.",R0),"If the population is vaccinated at or above the critical fraction",varfmt("pc",1-1/R0),"then an oubreak will not occur.",sir_caption_p(tmax),collapse=" ")
      
    )
    
    return(list(plots,indexes,titles,captions))
  }
  
  # This is messy but I don't know how else to do it.
  output$title1a = renderPrint({
    index = p1()[[2]][[1]]
    title = p1()[[3]][[1]]
    cat(titlefmt(index,title))
  })
  output$plot1 = renderPlot({
    p1()[[1]][[1]]
  })
  output$caption1 = renderPrint({ 
    cat(paste("<p>",p1()[[4]][[1]],"</p>",sep=""))
  })
  output$title2a <- renderPrint({
    index = p2()[[2]][[1]]
    title = p2()[[3]][[1]]
    cat(titlefmt(index,title))
  })
  output$title2b <- renderPrint({
    index = p2()[[2]][[2]]
    title = p2()[[3]][[2]]
    cat(titlefmt(index,title))
  })
  output$title2c <- renderPrint({
    index = p2()[[2]][[3]]
    title = p2()[[3]][[3]]
    cat(titlefmt(index,title))
  })
  output$title2d <- renderPrint({
    index = p2()[[2]][[4]]
    title = p2()[[3]][[4]]
    cat(titlefmt(index,title))
  })
  output$title2e <- renderPrint({
    index = p2()[[2]][[5]]
    title = p2()[[3]][[5]]
    cat(titlefmt(index,title))
  })
  output$title2f <- renderPrint({
    index = p2()[[2]][[6]]
    title = p2()[[3]][[6]]
    cat(titlefmt(index,title))
  })
  output$plot2a <- renderPlot({
    p2()[[1]][[1]]
  })
  output$plot2b <- renderPlot({
    p2()[[1]][[2]]
  })
  output$plot2c <- renderPlot({
    p2()[[1]][[3]]
  })
  output$plot2d <- renderPlot({
    p2()[[1]][[4]]
  })
  output$plot2e <- renderPlot({
    p2()[[1]][[5]]
  })
  output$plot2f <- renderPlot({
    p2()[[1]][[6]]
  })
  output$caption2a = renderPrint({ 
    cat(paste("<p>",p2()[[4]][[1]],"</p>",sep=""))
  })
  output$caption2b = renderPrint({ 
    cat(paste("<p>",p2()[[4]][[2]],"</p>",sep=""))
  })
  output$caption2c = renderPrint({ 
    cat(paste("<p>",p2()[[4]][[3]],"</p>",sep=""))
  })
  output$caption2d = renderPrint({ 
    cat(paste("<p>",p2()[[4]][[4]],"</p>",sep=""))
  })
  output$caption2e = renderPrint({ 
    cat(paste("<p>",p2()[[4]][[5]],"</p>",sep=""))
  })
  output$caption2f = renderPrint({ 
    cat(paste("<p>",p2()[[4]][[6]],"</p>",sep=""))
  })
  output$title3a <- renderPrint({
    index = p3()[[2]][[1]]
    title = p3()[[3]][[1]]
    cat(titlefmt(index,title))
  })
  output$title3b <- renderPrint({
    index = p3()[[2]][[2]]
    title = p3()[[3]][[2]]
    cat(titlefmt(index,title))
  })
  output$title3c <- renderPrint({
    index = p3()[[2]][[3]]
    title = p3()[[3]][[3]]
    cat(titlefmt(index,title))
  })
  output$title3d <- renderPrint({
    index = p3()[[2]][[4]]
    title = p3()[[3]][[4]]
    cat(titlefmt(index,title))
  })
  output$plot3a <- renderPlot({
    p3()[[1]][[1]]
  })
  output$plot3b <- renderPlot({
    p3()[[1]][[2]]
  })
  output$plot3c <- renderPlot({
    p3()[[1]][[3]]
  })
  output$plot3d <- renderPlot({
    p3()[[1]][[4]]
  })
  output$caption3a = renderPrint({ 
    cat(paste("<p>",p3()[[4]][[1]],"</p>",sep=""))
  })
  output$caption3b = renderPrint({ 
    cat(paste("<p>",p3()[[4]][[2]],"</p>",sep=""))
  })
  output$caption3c = renderPrint({ 
    cat(paste("<p>",p3()[[4]][[3]],"</p>",sep=""))
  })
  output$caption3d = renderPrint({ 
    cat(paste("<p>",p3()[[4]][[4]],"</p>",sep=""))
  })
  
}