plots1 = function(params) { #R0,ip,lp,id,le,alpha=0,p=0) {

  out     = df1()
  df      = out[[1]]
  dfsir   = out[[2]]
 
  plot1   = ggplot(data=df,aes(x=time)) # S,E,I,R 
  plot2   = ggplot(data=df)             # I vs S
  
  tmax    = max(df$time)
  
  # minimum I that is above S(0), this is for setting minimum y-axis values for log plots
  t0      = df[df$I > sir_init_i,]$time[1] 
  imin    = min(df[df$time > t0,]$I)

  # minimum S for phase plane
  smin    = min(df$S)
  
  # include SIR trace
  if(input$sir1) {
    smin  = min(smin,min(dfsir$S))
    plot1 = plot1 + geom_line(data=dfsir,mapping=aes(x=time,y=I,colour="C3"),size=plot_line_width)
    plot2 = plot2 + geom_path(data=dfsir,mapping=aes(x=S,y=I,colour="C2"),size=plot_line_width)
  }
  
  plot1 = plot1 + geom_line(aes(y=S,colour="S"),size=plot_line_width)
  plot1 = plot1 + geom_line(aes(y=E,colour="E"),size=plot_line_width)
  plot1 = plot1 + geom_line(aes(y=R,colour="R"),size=plot_line_width)
  plot1 = plot1 + geom_line(aes(y=I,colour="I"),size=plot_line_width)
  
  plot1 = plot1 + geom_hline(yintercept=params$stars$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_hline(yintercept=params$stars$E,colour=palette["E"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_hline(yintercept=params$stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_hline(yintercept=params$stars$R,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  
  plot2 = plot2 + geom_path(aes(x=S,y=I,colour="C1"),size=plot_line_width)
  plot2 = plot2 + geom_path(data=df %>% filter(time <= params$period),mapping=aes(x=S,y=I,colour="M"),size=plot_line_width)
  plot2 = plot2 + geom_hline(yintercept=params$stars$I,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot2 = plot2 + geom_vline(xintercept=params$stars$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  
  # points every year
  if(input$points1 == TRUE) {
    df$quarter = trunc(df$time/(365/4))
    quarterly      = df[!duplicated(df$quarter) & df$quarter,]
    n_periods      = 3
    point_size_mag = 1.5
    plot1 = plot1 + geom_point(data=quarterly %>% filter(quarter%%4 == 0 & time <= params$period*n_periods),mapping=aes(x=time,y=I),colour=palette["I"],fill=palette["I"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot1 = plot1 + geom_point(data=quarterly %>% filter(quarter%%4 > 0  & time <= params$period*n_periods),mapping=aes(x=time,y=I),colour=palette["I"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot2 = plot2 + geom_point(data=quarterly %>% filter(quarter%%4 == 0 & time <= params$period),mapping=aes(x=S,y=I),colour=palette["M"],fill=palette["M"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot2 = plot2 + geom_point(data=quarterly %>% filter(quarter%%4 > 0  & time <= params$period),mapping=aes(x=S,y=I),colour=palette["M"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot2 = plot2 + geom_point(data=quarterly %>% filter(quarter%%4 == 0 & time > params$period & time <= params$period*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill=palette["C1"],size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
    plot2 = plot2 + geom_point(data=quarterly %>% filter(quarter%%4 > 0  & time > params$period & time <= params$period*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="white",size=point_size_mag*plot_line_width,stroke=plot_line_width,shape=21)
  }

  param_text = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>&beta;</i> = %.2f days, 1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day with %.0f%% of the population vaccinated.",
                       params$R0,
                       params$beta,
                       params$ip,
                       params$lp,
                       params$id /365,
                       params$le /365,
                       params$alpha,
                       params$p * 100)

  title1 = sprintf("SEIRS trajectories of infection spread for %s",param_text)
  title2 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for %s",param_text)
  
  plot1 = plot1 + my.plot_legend + my.plot_axis(xlab="days",ylab="fraction of population (%)",
                                                ylog10min=min(sir_init_i,imin),
                                                xmin=0,xmax=tmax,
                                                log10=input$log1)
  plot2 = plot2 + my.plot_legend + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                xmin=smin,xmax=1,
                                                ylog10min=min(sir_init_i,imin),
                                                dlog10=input$log1,
                                                ysec=0,xpercent=1)
  
    caption1 = paste("The SEIRS model trajectories for susceptible (S), exposed (E), infected (I) and recovered (R) groups for",param_text," Horizontal dashed lines represent endemic equilibrium values for each trajectory.")

    caption2 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",param_text,"Horizontal dashed lines represent endemic equilibrium values. The trajectory over the first inter-epidemic interval",varfmt("t ≤ T_E",params$period/365,prec=2,units="years"),"is shown in magenta.")
    
  if(input$points1 == TRUE) {
    caption1 = paste(caption1,"Points on the trajectory of",varfmt("I"),"indicate time in steps of 1 year (solid) or one quarter (hollow) over the first",n_periods,"inter-epidemic intervals",varfmt("T_E.",params$period/365,units="years",prec=2))
    caption2 = paste(caption2,"Points on the trajectory indicate time in steps of 1 year (solid) or one quarter (hollow) over the first",n_periods,"inter-epidemic intervals",varfmt("T_E.",params$period/365,units="years",prec=2))
  }
  if(input$sir1) {
    caption1 = paste(caption1,"Grey trace shows infected trajectory of a closed epidemic from the SIR model with the same parameters.")
    caption2 = paste(caption2,"Grey trace shows trajectory of a closed epidemic from the SIR model with the same parameters.")
  }
    
  caption1 = paste(caption1,sir_caption(tmax,params$p))
  caption2 = paste(caption2,sir_caption(tmax,params$p))
  
  if(input$sir1) {
    caption1 = paste(caption1,"SIR trajectory was computed similarly with the same",varfmt("R0"),"and",varfmt("gamma."))
    caption2 = paste(caption2,"SIR trajectory was computed similarly with the same",varfmt("R0"),"and",varfmt("gamma."))
  }
  
  return(list(list(plot_theme(plot1),plot_theme(plot2)),list(1,2),list(title1,title2),list(caption1,caption2)))
}
