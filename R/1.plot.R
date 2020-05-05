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
