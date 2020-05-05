
# Panel 1: trajectories for a given R0, ip and vac
calculate1 = function(R0,ip,lp,id,le,vac) {
  gamma = 1/ip
  sigma = 1/lp
  omega = 1/(id*365)
  mu    = 1/(le*365)
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  tmax  = seirs_t_bound(beta,gamma,sigma,omega,mu,vac)
  out   = seirs(R0=R0, gamma=gamma, sigma=sigma, omega=omega, mu=mu, vac=vac, tmax=tmax, step=tmax/sir_system_steps)
  return(out)
}

# Panel 2: step over R0 from Rinit to R0final
calculate2 = function(R0init,R0final,ip) {
  R0step   = 0.1 * ((R0final>R0init)*2-1)
  R0_range = seq(R0init,R0final,length.out=5)
  summary  = data.frame()
  gamma    = 1/ip
  vac      = 0
  t0 = Sys.time()
  tmax     = sir_t_bound(gamma*min(R0_range),gamma,vac=vac)
  trajectories = data.frame()
  for (R0 in R0_range) {
    beta  = gamma * R0
    df    = sir(beta = beta, gamma = gamma, vac = vac, tmax=tmax, step = tmax/sir_system_steps)
    trajectories = rbind(trajectories,df)
    imax  = df[df$I == max(df$I),]$I
    rmax  = df[nrow(df),]$R
    smin  = df[nrow(df),]$S
    timax = df[df$I == max(df$I),]$time
    summary = rbind(summary,data.frame(R0=R0,timax=timax,smin=smin,imax=imax,rmax=rmax))
  }
  t1 = Sys.time()
  report_timing(t0,t1)
  return(list(trajectories,summary))
}

# Panel 3: step 0 to p_c for a given R0
calculate3  = function(R0,ip) {
  gamma     = 1/ip
  beta      = gamma*R0
  pc_crit   = 1-1/R0
  vac_range = seq(0,pc_crit-0.001,by=0.1)
  t0 = Sys.time()
  tmax      = sir_t_bound(gamma*R0,gamma,max(vac_range))
  summary   = data.frame()
  trajectories = data.frame()
  for (vac in vac_range) {
    df    = sir(beta = beta, gamma = gamma, vac = vac, tmax = tmax, step = tmax/sir_system_steps)
    trajectories = rbind(trajectories,df)
    imax  = df[df$I == max(df$I),]$I
    rmax  = df[nrow(df),]$R
    smin  = df[nrow(df),]$S
    timax = df[df$I == max(df$I),]$time
    summary = rbind(summary,data.frame(R0=R0,vac=vac,timax=timax,smin=smin,imax=imax,rmax=rmax))
  }
  t1 = Sys.time()
  report_timing(t0,t1)
  return(list(trajectories,summary))
}
