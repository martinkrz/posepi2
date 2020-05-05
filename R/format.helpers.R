varfmt = function(name=NULL,value=NULL,prec=1,percent=0,comma=0,units="") {
  trailing = ""
  if(is.null(name)) {
    fmtstr = sprintf("%%.%df",prec)
    if(percent) {
      fmtstr = paste(fmtstr,"%%",sep="")
    }
    if(units != "") {
      fmtstr = paste(fmtstr,units,sep=" ")
    }
    if(percent) {
      value = 100 * value
    }
    str    = sprintf(fmtstr,value)
    return(str)
  }
  rx = str_match_all(name,"^(.+)([.,])$")
  if(length(rx[[1]]) == 3) {
    name = rx[[1]][[2]]
    trailing = rx[[1]][[3]]
  }
  if(name == "ip") {
    name    = HTML("<i>ip</i>")
    prec    = 0
    units   = "days"
  } else if (name == "R0") {
    name    = HTML("<i>R</i><sub>0</sub>")
    prec    = 1
  } else if (name == "S(0)") {
    name    = HTML("<i>S</i>(0)")
  } else if (name == "I(0)") {
    name    = HTML("<i>I</i>(0)")
  } else if (name == "R(0)") {
    name    = HTML("<i>R</i>(0)")
  } else if (name == "pc") {
    name    = HTML("<i>p</i><sub>c</sub>")
    prec    = 0
    percent = 1
  } else if (name == "p") {
    name    = HTML("<i>p</i>")
    prec    = 0
    percent = 1
  } else if (name == "Imax") {
    name    = HTML("<i>I</i><sub>max</sub>")
    prec    = 1
    percent = 1
  } else if (name == "t") {
    name    = HTML("<i>t</i>")
    prec    = 1
    units   = "days"
  } else if (name == "C") {
    name    = HTML("<i>C</i>")
    prec    = 0
    percent = 1
  } else if (name == "t1") {
    name    = HTML("<i>t</i><sub>1</sub>")
    prec    = 1
    units   = "days"
  } else if (name == "t2") {
    name    = HTML("<i>t</i><sub>2</sub>")
    prec    = 1
    units   = "days"
  } else if (name == "deltat") {
    name    = HTML("Δ<i>t</i>")
    prec    = 1
    units   = "days"
  } else if (name == "Ideltat") {
    name    = HTML("Σ<i>I</i>(Δ<i>t</i>)")
    prec    = 1
    percent = 1
    units   = "cases"
  } else if (name == "I>deltat") {
    name    = HTML("Σ<i>I</i><sub>&gt;</sub>(Δ<i>t</i>)")
    prec    = 1
    percent = 1
    units   = "cases"
  } else if (name == "tmax") {
    name    = HTML("<i>t</i><sub>max</sub>")
    prec    = 1
    units   = "days"
  } else if (name == "tmax") {
    name    = HTML("<i>t</i><sub>max</sub>")
    prec    = 1
    units   = "days"
  } else if (name == "Stmax") {
    name    = HTML("<i>S</i>(<i>t</i><sub>max</sub>)")
    prec    = 1
    percent   = 1
  } else if (name == "Rtmax") {
    name    = HTML("<i>R</i>(<i>t</i><sub>max</sub>)")
    prec    = 1
    percent  = 1
  } else if (name == "Sinf") {
    name    = HTML("<i>S</i>(∞)")
    prec    = 1
    percent   = 1
  } else if (name == "Rinf") {
    name    = HTML("<i>R</i>(∞)")
    prec    = 1
    percent = 1
  } else if (name == "beta") {
    name = HTML("<i>β</i>")
  } else if (name == "gamma") {
    name = HTML("<i>γ</i>")
  } else if (name == "sigma") {
    name = HTML("<i>&sigma;</i>")
  } else if (name == "omega") {
    name = HTML("<i>&omega;</i>")
  } else if (name == "mu") {
    name = HTML("<i>&mu;</i>")
  }
  if(! is.null(value)) {
    if(comma) {
      fmtstr = sprintf("%%s = %%s")
    } else {
      fmtstr = sprintf("%%s = %%.%df",prec)
    }
    if(percent) {
      fmtstr = paste(fmtstr,"%%",sep="")
    }
    if(units != "") {
      fmtstr = paste(fmtstr,units,sep=" ")
    }
    if(percent) {
      value = 100 * value
    }
    if(comma) {
      value = formatC(value,format="f",big.mark=",",digits=prec)
    }
    str    = sprintf(fmtstr,name,value)
  } else {
    str = name
  }
  if(trailing != "") {
    str = paste(str,trailing,sep="")
  }
  return(str)
}

titlefmt = function(index,title) {
  sprintf("<h5><b>Supplemental Figure %d</b><br><p>%s</p>",index,title)
}

label_to_percent = function(str) {
  str = sprintf("%f",str*100)
  parse(text=str)
}

ceilToFraction = function(num, den = 1) {
  x = den * ceiling( num / den)
  return(x)
}

floorToFraction = function(num, den = 1) {
  x = den * floor(num / den)
  return(x)
}

table = function(rows,title=NULL) {
  cat("<div class=parameters>")
  if(! is.null(title)) {
    cat(paste("<div>",title,"</div>"))
  }
  cat(paste("<table>"))
  for(i in 1:nrow(rows)) {
    row = rows[i,]
    cat(paste("<tr><td>",row$name,"</td><td>",row$value,"</td></tr>",sep=""))
  }
  cat("</table></div>")
}

makerows = function(items) {
  rows = data.frame()
  for(i in seq(1,length(items),by=2)) {
    rows = rbind(rows,data.frame(name=varfmt(items[i]),value=items[i+1]))
  }
  return(rows)
}

