output$masthead = renderPrint({ 
  cat(paste("<div id=natmeth><img src='img/nature.methods.png'/></div>",sep=""))
  cat(paste("<div id=mast><b>Supplemental Materials |</b> Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. Points of Significance: Adding realism to the SIR model for infectious disease epidemics. (2020) <i>Nat Meth</i> <b>17</b> (in press).</div>",sep=""))
})

sir_caption = function(tmax,p) {
  paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t"),"= 0 to",varfmt("t"),"=",formatC(tmax,format="f",big.mark=",",digits=0),"days in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 with",varfmt("Szero,",1-sir_init_i-p,prec=3),varfmt("Ezero",sir_init_i),varfmt("Izero,",sir_init_i,prec=3)," and",varfmt("Rzero.",p,prec=3),sep=" ")
}

sir_caption_p = function(tmax) {
  paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",formatC(tmax,format="f",big.mark=",",digits=0)),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 to",varfmt("S(0)",1-sir_init_i,prec=3)," &ndash; <i>p</i>,",varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0)")," = <i>p</i> for vaccination fraction",varfmt("p."),sep=" ")
}
