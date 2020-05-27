plots2 = function(params1,params2) {

  out     = df2()
  df1     = out[[1]]
  df2     = out[[2]]
  dfsir   = out[[3]]

  tmax     = max(df1$time,df2$time)
  
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
  plot1 = plot1 + geom_path(data=df1 %>% filter(time <= params1$period),mapping=aes(x=S,y=I,colour="M"),size=plot_line_width)
  plot1 = plot1 + geom_hline(yintercept=params1$stars$I,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_vline(xintercept=params1$stars$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  
  plot1 = plot1 + geom_path(data=df2,mapping=aes(x=S,y=I,colour="CD"),size=plot_line_width)
  plot1 = plot1 + geom_path(data=df2 %>% filter(time <= params2$period),mapping=aes(x=S,y=I,colour="C"),size=plot_line_width)
  plot1 = plot1 + geom_hline(yintercept=params2$stars$I,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_vline(xintercept=params2$stars$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  
  # points every year
  if(input$points2 == TRUE) {
    n_periods      = 3
    point_size_mag = 1.5
    
    df1$quarter     = trunc(df1$time/(365/4))
    df2$quarter     = trunc(df2$time/(365/4))
    quarterly1      = df1[!duplicated(df1$quarter) & df1$quarter,]
    quarterly2      = df2[!duplicated(df2$quarter) & df2$quarter,]
    
    plot1 = plot1 + geom_point(data=quarterly1 %>% filter(quarter%%4 == 0 & time <= params1$period),mapping=aes(x=S,y=I),colour=palette["M"],fill=palette["M"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly1 %>% filter(quarter%%4 > 0  & time <= params1$period),mapping=aes(x=S,y=I),colour=palette["M"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly1 %>% filter(quarter%%4 == 0 & time > params1$period & time <= params1$period*n_periods),mapping=aes(x=S,y=I),colour=palette["MD"],fill=palette["MD"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly1 %>% filter(quarter%%4 > 0  & time > params1$period & time <= params1$period*n_periods),mapping=aes(x=S,y=I),colour=palette["MD"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    
    plot1 = plot1 + geom_point(data=quarterly2 %>% filter(quarter%%4 == 0 & time <= params2$period),mapping=aes(x=S,y=I),colour=palette["C"],fill=palette["C"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly2 %>% filter(quarter%%4 > 0  & time <= params2$period),mapping=aes(x=S,y=I),colour=palette["C"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly2 %>% filter(quarter%%4 == 0 & time > params2$period & time <= params2$period*n_periods),mapping=aes(x=S,y=I),colour=palette["CD"],fill=palette["CD"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly2 %>% filter(quarter%%4 > 0  & time > params2$period & time <= params2$period*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    
  }
  #cat(paste(lp1,id1,lp2,id2,R0,beta1))
  param1_text = sprintf("<i>R</i><sub>0,1</sub> = %.1f, 1/<i>&beta;</i><sub>1</sub> = %.2f days, 1/<i>&gamma;</i><sub>1</sub> = %d days, 1/<i>&sigma;</i><sub>1</sub> = %d days, 1/<i>&omega;</i><sub>1</sub> = %.1f years, 1/<i>&mu;</i><sub>1</sub> = %d years and 1/<i>&alpha;</i><sub>1</sub> = %.0f days with <i>p</i><sub>1</sub> = %.0f%% of the population vaccinated",
                       params1$R0,
                       params1$beta,
                       params1$ip,
                       params1$lp,
                       params1$id/365,
                       params1$le/365,
                       params1$alpha,
                       params1$p*100)
  param2_text = sprintf("<i>R</i><sub>0,2</sub> = %.1f, 1/<i>&beta;</i><sub>2</sub> = %.2f days, 1/<i>&gamma;</i><sub>2</sub> = %d days, 1/<i>&sigma;</i><sub>2</sub> = %d days, 1/<i>&omega;</i><sub>2</sub> = %.1f years, 1/<i>&mu;</i><sub>2</sub> = %d years and 1/<i>&alpha;</i><sub>2</sub> = %.0f days with <i>p</i><sub>2</sub> = %.0f%% of the population vaccinated",
                        params2$R0,
                        params2$beta,
                        params2$ip,
                        params2$lp,
                        params2$id/365,
                        params2$le/365,
                        params2$alpha,
                        params2$p*100)
  
  title1 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for two scenarios: %s %s and %s %s.",HTML("<span class=s1>1</span>"),param1_text,HTML("<span class=s2>2</span>"),param2_text)
  
  plot1 = plot1 + my.plot_legend2 + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                xmin=smin,xmax=1,
                                                ylog10min=min(sir_init_i,imin),
                                                dlog10=input$log2,
                                                ysec=0,xpercent=1)
  
    caption1 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model for two scenarios:",HTML("<span class=s1>1</span>"),param1_text,"and",HTML("<span class=s2>2</span>"),paste(param2_text,".",sep=""),"Horizontal dashed lines represent endemic equilibrium values. For each scenario, the trajectory over the first inter-epidemic interval",varfmt("t ≤ T_E",params1$period/365,prec=2,units="years"),"is shown in a brighter color.")
    
  if(input$points2 == TRUE) {
    caption1 = paste(caption1,"Points on the trajectory indicate time in steps of 1 year (solid) or one quarter (hollow) over the first",n_periods,"inter-epidemic intervals",varfmt("T_E.",params1$period/365,units="years",prec=2))
  }
  if(input$sir2) {
    caption1 = paste(caption1,"Grey trace shows infected trajectory of a closed epidemic from the SIR model with the same parameters.")
  }
    
  caption1 = paste(caption1,sir_caption(tmax,params1$p))

  if(input$sir1) {
    caption1 = paste(caption1,"SIR trajectory was computed similarly with the same",varfmt("R0",params1$R0),"and",varfmt("1/gamma",params1$ip,prec=0,units="days"),"as the first trajectory.")
  }
  
  return(list(list(plot_theme(plot1)),list(3),list(title1),list(caption1)))
  
}
