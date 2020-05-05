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