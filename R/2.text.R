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
