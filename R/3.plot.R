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