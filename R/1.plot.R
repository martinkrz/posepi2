plots1 = function(R0,ip,lp,id,le,p) {

  df      = df1()
  plot1   = ggplot(data=df,aes(x=time)) # S,E,I,R 
  plot2   = ggplot(data=df) # I vs S
  df$quarter = trunc(df$time/(365/4))
  imax    = max(df$I)
  rmax    = df[nrow(df),]$R
  timax   = df[df$I == max(df$I),]$time
  tmax    = max(df$time)

  # minimum I that is above S(0), this is for setting minimum y-axis values for log plots
  t0      = df[df$I > sir_init_i,]$time[1] 
  imin    = min(df[df$time > t0,]$I)
  #cat(paste(t0,imin))
  
  tmp    = df[df$I >= max(df$I)/2,]
  t1max  = tmp[1,]$time
  it1max = tmp[1,]$I
  t2max  = tmp[nrow(tmp),]$time
  it2max = tmp[nrow(tmp),]$I
  
  gamma = 1/ip
  sigma = 1/lp
  omega = 1/(365*id)
  mu    = 1/(365*le)
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  stars = stars(R0,gamma,sigma,omega,mu)
  period = ieperiod(R0,gamma,sigma,omega,mu)
  
  plot1 = plot1 + geom_line(aes(y=S,colour="S"),size=plot_line_width)
  plot1 = plot1 + geom_line(aes(y=E,colour="E"),size=plot_line_width)
  plot1 = plot1 + geom_line(aes(y=R,colour="R"),size=plot_line_width)
  plot1 = plot1 + geom_line(aes(y=I,colour="I"),size=plot_line_width)
  plot2 = plot2 + geom_path(aes(x=S,y=I,colour="C1"),size=plot_line_width)
  
  plot1 = plot1 + geom_hline(yintercept=stars$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_hline(yintercept=stars$E,colour=palette["E"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_hline(yintercept=stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot1 = plot1 + geom_hline(yintercept=stars$R,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  
  plot2 = plot2 + geom_hline(yintercept=stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot2 = plot2 + geom_vline(xintercept=stars$S,colour=palette["S"],size=plot_line_width/2,linetype="dashed")
  
  # points every year
  quarterly = df[!duplicated(df$quarter) & df$quarter,]
  n_periods = 3
  plot1 = plot1 + geom_point(data=quarterly %>% filter(quarter%%4 == 0 & time <= period*n_periods),mapping=aes(x=time,y=I),colour=palette["I"],size=2.5*plot_line_width)
  plot1 = plot1 + geom_point(data=quarterly %>% filter(quarter%%4 > 0  & time <= period*n_periods),mapping=aes(x=time,y=I),colour=palette["I"],fill="white",size=2.5*plot_line_width,stroke=plot_line_width,shape=21)
  
  plot2 = plot2 + geom_point(data=quarterly %>% filter(quarter%%4 == 0 & time <= period*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],size=2.5*plot_line_width)
  plot2 = plot2 + geom_point(data=quarterly %>% filter(quarter%%4 > 0  & time <= period*n_periods),mapping=aes(x=S,y=I),colour=palette["C1"],fill="white",size=2.5*plot_line_width,stroke=plot_line_width,shape=21)
  
  #plot1 = plot1 + geom_hline(yintercept=imax,colour=as.list(palette)$I,size=plot_line_width/2,linetype="dashed")
  #plot1 = plot1 + geom_hline(yintercept=rmax,colour=as.list(palette)$R,size=plot_line_width/2,linetype="dashed")
  #plot1 = plot1 + geom_vline(xintercept=timax,colour=as.list(palette)$I,size=plot_line_width/2,linetype="dotted")
  #plot = plot + geom_line(data=data.frame(x=c(t1max,t1max),y=c(0,it1max)),aes(x,y),colour=as.list(palette)$I,size=plot_line_width/2,linetype="solid")
  #plot = plot + geom_line(data=data.frame(x=c(t2max,t2max),y=c(0,it2max)),aes(x,y),colour=as.list(palette)$I,size=plot_line_width/2,linetype="solid")
  
  title1 = sprintf("Infection spread for <i>R</i><sub>0</sub> = %.1f, 1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years and 1/<i>&mu;</i> = %d  years with %.0f%% vaccination.",R0,ip,lp,id,le,100*p)
  title2 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for <i>R</i><sub>0</sub> = %.1f, 1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years and 1/<i>&mu;</i> = %d  years with %.0f%% vaccination.",R0,ip,lp,id,le,100*p)
  
  
  plot1 = plot1 + my.plot_legend + my.plot_axis(xlab="days",ylab="fraction of population (%)",
                                                ylog10min=min(sir_init_i,imin),xmin=0,xmax=tmax,log10=input$log1)
  plot2 = plot2 + my.plot_legend + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                ylog10min=min(sir_init_i,imin),dlog10=input$log1,ysec=0,xpercent=1)
  
  caption1 = paste("The SEIRS model trajectories for susceptible (S), exposed (E), infected (I) and recovered (R) groups for",varfmt("R0,",R0),varfmt("1/gamma,",ip,units="days",prec=0),varfmt("1/sigma,",lp,units="days",prec=0),varfmt("1/omega,",id,units="years",prec=1),varfmt("1/mu,",le,units="years",prec=0),varfmt("beta,",beta,prec=2,units="/day"),"with",varfmt("p",p),"of the population vaccinated. Horizontal dashed lines represent endemic equilibrium values for each trajectory.",sir_caption(tmax,p),sep=" ")
  caption2 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",varfmt("R0,",R0),varfmt("1/gamma,",ip,units="days",prec=0),varfmt("1/sigma,",lp,units="days",prec=0),varfmt("1/omega,",id,units="years",prec=1),varfmt("1/mu,",le,units="years",prec=0),varfmt("beta,",beta,prec=2,units="/day"),"with",varfmt("p",p),"of the population vaccinated. Horizontal dashed lines represent endemic equilibrium values. Solid points indicate time in steps of 1 year (solid) or one quarter (hollow) over the first two inter-epidemic intervals,",varfmt("T_E.",period*2/365,units="years",prec=1),sir_caption(tmax,p),sep=" ")
  
  return(list(list(plot_theme(plot1),plot_theme(plot2)),list(1,2),list(title1,title2),list(caption1,caption2)))
}
