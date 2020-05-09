plots2 = function(R0,ip,lp1,id1,le,p,lp2,id2,alpha=0) {

  out     = df2()
  df1     = out[[1]]
  df2     = out[[2]]
  dfsir   = out[[3]]
  gamma   = 1/ip
  mu      = 1/(365*le)
  sigma1  = 1/lp1
  omega1  = 1/(365*id1)
  sigma2  = 1/lp2
  omega2  = 1/(365*id2)
  
  beta1   = beta(R0,gamma,sigma1,mu,model="seirs")
  beta2   = beta(R0,gamma,sigma2,mu,model="seirs")
  
  stars1  = stars(R0,gamma,sigma1,omega1,mu)
  period1 = ieperiod(R0,gamma,sigma2,omega2,mu,p=p)

  stars2   = stars(R0,gamma,sigma2,omega2,mu)
  period2  = ieperiod(R0,gamma,sigma2,omega2,mu,p=p)
    
  tmax    = max(df$time)
  
  # minimum I that is above S(0), this is for setting minimum y-axis values for log plots
  t0      = df1[df1$I > sir_init_i,]$time[1] 
  imin1   = min(df1[df1$time > t0,]$I)

  t0      = df2[df2$I > sir_init_i,]$time[1] 
  imin2   = min(df2[df2$time > t0,]$I)
  
  imin = min(imin1,imin2)
  
  # minimum S for phase plane
  smin    = min(df1$S,df2$S)
  
  plot1   = ggplot()
  
  # include SIR trace
  if(input$sir2) {
    smin  = min(smin,min(dfsir$S))
    #plot1 = plot1 + geom_line(data=dfsir,mapping=aes(x=time,y=I,colour="C3"),size=plot_line_width)
    plot1 = plot1 + geom_path(data=dfsir,mapping=aes(x=S,y=I,colour="C2"),size=plot_line_width)
  }
  
  plot1 = plot1 + geom_path(data=df1,aes(x=S,y=I,colour="MD"),size=plot_line_width)
  plot1 = plot1 + geom_path(data=df1 %>% filter(time <= period1),mapping=aes(x=S,y=I,colour="M"),size=plot_line_width)
  plot1 = plot1 + geom_hline(yintercept=stars1$I,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_vline(xintercept=stars1$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  
  plot1 = plot1 + geom_path(data=df2,mapping=aes(x=S,y=I,colour="CD"),size=plot_line_width)
  plot1 = plot1 + geom_path(data=df2 %>% filter(time <= period2),mapping=aes(x=S,y=I,colour="C"),size=plot_line_width)
  plot1 = plot1 + geom_hline(yintercept=stars2$I,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_vline(xintercept=stars2$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  
  # points every year
  if(input$points2 == TRUE) {
    n_periods      = 3
    point_size_mag = 1.5
    
    df1$quarter     = trunc(df1$time/(365/4))
    df2$quarter     = trunc(df2$time/(365/4))
    quarterly1      = df1[!duplicated(df1$quarter) & df1$quarter,]
    quarterly2      = df2[!duplicated(df2$quarter) & df2$quarter,]
    
    plot1 = plot1 + geom_point(data=quarterly1 %>% filter(quarter%%4 == 0 & time <= period1*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="black",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly1 %>% filter(quarter%%4 > 0  & time <= period1*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    
    plot1 = plot1 + geom_point(data=quarterly2 %>% filter(quarter%%4 == 0 & time <= period2*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="black",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly2 %>% filter(quarter%%4 > 0  & time <= period2*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    
  }
  cat(paste(lp1,id1,lp2,id2,R0,beta1))
  param_text = sprintf("<i>R</i><sub>0</sub> = %.1f, 1/<i>&beta;</i> = %.2f days, 1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.0f with %.0f%% of the population vaccinated.",R0,beta1,ip,lp1,id1,le,alpha,100*p)
  
  title1 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for %s",param_text)
  
  plot1 = plot1 + my.plot_legend + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                xmin=smin,xmax=1,
                                                ylog10min=min(sir_init_i,imin),
                                                dlog10=input$log2,
                                                ysec=0,xpercent=1)
  
    caption1 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",param_text,"Horizontal dashed lines represent endemic equilibrium values. The trajectory over the first inter-epidemic interval",varfmt("t ≤ T_E",period1/365,prec=2,units="years"),"is shown in magenta.")
    
  if(input$points2 == TRUE) {
    caption1 = paste(caption1,"Points on the trajectory indicate time in steps of 1 year (solid) or one quarter (hollow) over the first",n_periods,"inter-epidemic intervals",varfmt("T_E.",period1/365,units="years",prec=2))
  }
  if(input$sir2) {
    caption1 = paste(caption1,"Grey trace shows infected trajectory of a closed epidemic from the SIR model with the same parameters.")
  }
    
  caption1 = paste(caption1,sir_caption(tmax,p))

  if(input$sir1) {
    caption1 = paste(caption1,"SIR trajectory was computed similarly with the same",varfmt("R0"),"and",varfmt("gamma."))
  }
  
  return(list(list(plot_theme(plot1)),list(3),list(title1),list(caption1)))
  
}
