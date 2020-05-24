multiplot  = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    suppressWarnings(print(plots[[1]]))
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      suppressWarnings(print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                       layout.pos.col = matchidx$col)))
    }
  }
}

plot_theme = function(p) {
  line_size   = 1
  tick_length = 5
  p + theme(plot.background  = element_blank(),
            #aspect.ratio     = 1,
            panel.border     = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(color="#dddddd",size=line_size/2.13),
            panel.background = element_blank(),
            line = element_line(color="black",size=line_size/2.13,lineend="round"),
            text = element_text(color="black",size=plot_text_size,hjust=0.5,vjust=0.5,angle=0,lineheight=0.9),
            axis.ticks        = element_line(color="black"),
            axis.ticks.length = unit(tick_length,"points"),
            axis.line         = element_line(),
            axis.title        = element_text(color="black",size=1.2*plot_text_size),
            axis.text         = element_text(color="black",size=plot_text_size),
            panel.spacing     = unit(c(0, 0,   0,   0), "points"),
            #axis.title.x     = element_blank(),
            #axis.title.y     = element_blank(),
            legend.position   = "bottom",
            legend.key        = element_blank(),
            legend.title      = element_text(size=plot_text_size,face="bold",color="#333333"),
            legend.text       = element_text(size=plot_text_size),
            plot.margin       = unit(c(0, 75, 0, 0), "points")
            #axis.title.x.top  = element_text(margin=margin(t=-3,unit="pt")),
            #axis.line.x.top  = element_line(margin=margin(t=-15,unit="pt"))
  )
}

my.plot_legend = list(
  scale_colour_manual("GROUP", 
                      breaks = c("S", "E", "I", "R",
                                 "C1",
                                 "MD",
                                 "CD",
                                 "M",
                                 "C",
                                 "C2",
                                 "C3"),
                      labels = c("susceptible","exposed","infected","recovered",
                                 "I vs S (SEIRS)",
                                 "Scenario 1",
                                 "Scenario 2",
                                 "first inter-epidemic interval",
                                 "first inter-epidemic interval",
                                 "I vs S (SIR)","infected (SIR)"),
                      values = palette)
)
my.plot_legend2 = list(
  scale_colour_manual("GROUP", 
                      breaks = c("M",
                                 "C",
                                 "C2"),
                      labels = c(
                                 "Scenario 1",
                                 "Scenario 2",
                                 "I vs S (SIR)"),
                      values = palette)
)  

axis_spacing = function(min=0,max=1,default=0.2) {
  spacing = default
  range   = max - min
  if(range <= 0.1) {
    spacing = 0.02
  } else if (range <= 0.2) {
    spacing = 0.05
  } else if (range <= 0.3) {
    spacing = 0.05
  } else if (range <= 0.5) {
    spacing = 0.1
  }
  return(spacing)
}

my.plot_axis = function(xlab="days",
                        ylab="percent of population (%)",
                        xmin    = NULL, xmax  = NULL,
                        ymin    = NULL, ymax  = NULL,
                        ylog10min = NA,
                        log10   = 0,    
                        dlog10  = 0,
                        ysec     = 1,
                        ypercent = 1,
                        xpercent = 0) {
  xlim = ! is.null(xmin) & ! is.null(xmax)
  ylim = ! is.null(ymin) & ! is.null(ymax)
  #cat(paste(xlim,ylim))
  if(ylim) {
    by   = axis_spacing(ymin,ymax)
  } else {
    
  }
  opt  = list()
  if(xpercent) {
    xfun = label_to_percent
  } else {
    xfun = label_to_identity
  }
  if(ypercent) {
    yfun = label_to_percent
  } else {
    yfun = label_to_identity
  }
  # secondary x axis
  if(ysec) {
    if(xlim) {
      opt = append(opt,scale_x_continuous(xlab,lim=c(xmin,xmax),labels=xfun,sec.axis=sec_axis( ~ . / 365,name="years")))
    } else {
      opt = append(opt,scale_x_continuous(xlab,labels=xfun,sec.axis=sec_axis( ~ . / 365,name="years")))
    }
  } else {
    if(xlim) {
      opt = append(opt,scale_x_continuous(xlab,lim=c(xmin,xmax),labels=xfun))
    } else {
      opt = append(opt,scale_x_continuous(xlab,labels=xfun))
    }
  }
  # log or double log axis
  if(log10 | dlog10) {
      br   = c(0.0001,0.0002,0.0005,0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5,1)
      if(ylim) {
        opt  = append(opt,scale_y_log10(ylab,lim=c(ymin,ymax),breaks=br,labels=yfun))
      } else {
        opt  = append(opt,scale_y_log10(ylab,lim=c(ylog10min,NA),breaks=br,labels=yfun))
      }
      if(dlog10) {
        br   = c(0.0001,0.0002,0.0005,0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        if(xlim) {
          cat()
          opt  = append(opt,scale_x_log10(xlab,lim=c(xmin,xmax),breaks=br,labels=xfun))
        } else {
          opt  = append(opt,scale_x_log10(xlab,breaks=br,labels=xfun))
        }
      }
  } else {
    if(ylim) {
      opt = append(opt,scale_y_continuous(ylab,lim=c(ymin,ymax),breaks=seq(ymin,ymax,by=by),labels=yfun))
    } else {
      opt = append(opt,scale_y_continuous(ylab,labels=yfun))
    }
  }
  return(opt)
}

my.plot_axis_both_percent = function(xlab="days",ylab="percent of population (%)",ymin=0,ymax=1,xmin=NULL,xmax=NULL) {
  ymax = min(1,ceilToFraction(ymax,0.05))
  ymin = max(0,floorToFraction(ymin,0.05))
  by   = axis_spacing(ymin,ymax)
  list(
    scale_x_continuous(xlab,lim=c(xmin,xmax),breaks=c(0,0.2,0.4,0.6,0.8,1),labels=label_to_percent),
    scale_y_continuous(ylab,lim=c(ymin,ymax),breaks=seq(ymin,ymax,by=by),labels=label_to_percent)
  )
}
