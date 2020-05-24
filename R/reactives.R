
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
  tmax1    = seirs_t_bound(params1)
  tmax2    = seirs_t_bound(params2)
  tmax     = max(tmax1,tmax2)
  seirs1   = seirs(params1,tmax=tmax,steps=sir_system_steps)
  seirs2   = seirs(params2,tmax=tmax,steps=sir_system_steps)
  sir_tmax = sir_t_bound(params1,tol=sir_init_i/2,tmax=1000) 
  sir      = sir(params1,tmax=max(sir_tmax,tmax))
  return(list(seirs1,seirs2,sir))
}

