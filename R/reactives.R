
# Panel 1: trajectories for a given R0, ip and vac
calculate1 = function(params) { # R0,ip,lp,id,le,alpha,p) {
  #params   = params()
  #print("calculate1")
  #print(params)
  #beta     = get_beta(p)
  tmax     = seirs_t_bound(params) #p$R0,p$gamma,p$sigma,p$omega,p$mu,p$vac)
  seirs    = seirs(params,tmax=tmax,steps=sir_system_steps) # R0=p$R0, gamma=p$gamma, sigma=p$sigma, omega=p$omega, mu=p$mu, vac=p$p, tmax=tmax,steps=sir_system_steps)
  sir_tmax = sir_t_bound(params,tol=sir_init_i/2,tmax=1000) # beta=p$R0*p$gamma,gamma=p$gamma,vac=p$p,tol=sir_init_i/2,tmax=1000)
  sir      = sir(params,tmax=sir_tmax) #beta=p$R0*p$gamma,gamma=p$gamma,vac=p$p,tmax=sir_tmax)
  return(list(seirs,sir))
}

# Panel 2: phase plane
calculate2 = function(params1,params2) {
  #gamma   = 1/ip
  #sigma1  = 1/lp1
  #omega1  = 1/(id1*365)
  #sigma2  = 1/lp2
  #omega2  = 1/(id2*365)
  #mu       = 1/(le*365)
  #beta1    = beta(R0,gamma,sigma1,mu,model="seirs")
  #beta2    = beta(R0,gamma,sigma2,mu,model="seirs")
  print("calculate2")
  print(params1)
  print(params2)
  tmax1    = seirs_t_bound(params1)
  tmax2    = seirs_t_bound(params2)
  tmax     = max(tmax1,tmax2)
  print(tmax)
  seirs1   = seirs(params1,tmax=tmax,steps=sir_system_steps)
  seirs2   = seirs(params2,tmax=tmax,steps=sir_system_steps)
  sir_tmax = sir_t_bound(params1,tol=sir_init_i/2,tmax=1000) 
  sir      = sir(params1,tmax=sir_tmax)
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
