
# Panel 1: trajectories for a given R0, ip and vac
calculate1 = function(R0,ip,lp,id,le,vac) {
  gamma  = 1/ip
  sigma  = 1/lp
  omega  = 1/(id*365)
  mu     = 1/(le*365)
  beta   = beta(R0,gamma,sigma,mu,model="seirs")
  tmax   = seirs_t_bound(R0,gamma,sigma,omega,mu,vac)
  seirs  = seirs(R0=R0, gamma=gamma, sigma=sigma, omega=omega, mu=mu, vac=vac, tmax=tmax,steps=sir_system_steps)
  sir_tmax = sir_t_bound(beta=R0*gamma,gamma=gamma,vac=vac,tol=sir_init_i/2,tmax=1000) 
  sir      = sir(beta=R0*gamma,gamma=gamma,vac=vac,tmax=sir_tmax)
  return(list(seirs,sir))
}

# Panel 2: phase plane
calculate2 = function(R0,ip,lp1,id1,le,vac,lp2,id2) {
  gamma   = 1/ip
  sigma1  = 1/lp1
  omega1  = 1/(id1*365)
  sigma2  = 1/lp2
  omega2  = 1/(id2*365)
  mu       = 1/(le*365)
  beta1    = beta(R0,gamma,sigma1,mu,model="seirs")
  beta2    = beta(R0,gamma,sigma2,mu,model="seirs")
  tmax1   = seirs_t_bound(R0,gamma,sigma1,omega1,mu,vac)
  tmax2    = seirs_t_bound(R0,gamma,sigma2,omega2,mu,vac)
  tmax = max(tmax1,tmax2)
  seirs1   = seirs(R0=R0, gamma=gamma, sigma=sigma1, omega=omega1, mu=mu, vac=vac, tmax=tmax,steps=sir_system_steps)
  seirs2   = seirs(R0=R0, gamma=gamma, sigma=sigma2, omega=omega2, mu=mu, vac=vac, tmax=tmax,steps=sir_system_steps)
  sir_tmax = sir_t_bound(beta=R0*gamma,gamma=gamma,vac=vac,tol=sir_init_i/2,tmax=1000) 
  sir      = sir(beta=R0*gamma,gamma=gamma,vac=vac,tmax=sir_tmax)
  return(list(seirs1,seirs2,sir))
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
