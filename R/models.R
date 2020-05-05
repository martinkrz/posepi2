
# estimate how long the outbreak lasts - this will be used as time axis max
sir_t_bound = function(beta,gamma,vac=0,tol=sir_init_i/2,tmax=1000,step=1) {
  # get a quick solution out to long time
  R0 = beta/gamma
  if(R0 <= 1 | vac >= 1-1/R0) {
    return(1000)
  }
  df     = sir(beta,gamma,vac,tmax=tmax,step=step)
  df_cut = df[df$I >= tol,]
  return( df_cut[nrow(df_cut),]$time )
}
# estimate how long the outbreak lasts - this will be used as time axis max
seirs_t_bound = function(R0,gamma,sigma,omega,mu,vac=0,tol=sir_init_i/2,tmax=1000,step=1) {
  # get a quick solution out to long time
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  if(R0 <= 1 | vac >= 1-1/R0) {
    return(1000)
  }
  df     = seirs(beta,gamma,sigma,omega,mu,vac,tmax=tmax,step=step)
  df_cut = df[df$I >= tol,]
  return(df_cut[nrow(df_cut),]$time)
}

seirs_system=function(t,x,params) {
  S=x[1]
  E=x[2]
  I=x[3]
  R=x[4]
  
  mu   = params["mu"]
  beta = params["beta"]
  omega= params["omega"]
  sigma= params["sigma"]
  gamma= params["gamma"]
  
  dS = -beta*S*I + omega*R  + mu     - mu*S 
  dE = -sigma*E  + beta*S*I          - mu*E
  dI = -gamma*I  + sigma*E           - mu*I
  dR = -omega*R  + gamma*I           - mu*R
  #     ^          ^          ^      ^
  #     outflow    inflow     birth  death
  res= c(dS, dE, dI, dR)
  list(res)
}

sir_system=function(t, x, parms){
  S = x[1]
  I = x[2]
  R = x[3]
  beta  = parms["beta"]
  mu    = parms["mu"]    # we're not using this here
  gamma = parms["gamma"]
  N     = parms["N"]
  dS    = mu*(N-S)  - beta*S*I/N
  dI    = beta*S*I/N - I*(mu + gamma)
  dR    = gamma*I - mu*R
  res   = c(dS,dI,dR)
  list(res)
}

# vac is the vaccination fraction - it is removed from S but not added to R
seirs = function(R0=3, gamma=1/14, sigma=1/7, omega=365/5, mu=365/76, vac=0, tmax=20, step=sir_system_time_step) {
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  time  = seq(0, tmax, by=step)
  parms = c(mu=mu, N=1, beta=beta, gamma=gamma, sigma=sigma, omega=omega, mu=mu)
  start = c(S=1-sir_init_i-vac, E=sir_init_i, I=0, R=0)
  out   = ode(y     = start, 
              times = time, 
              func  = seirs_system, 
              parms = parms)
  out   = cbind(out,data.frame(R0=beta/gamma,vac=vac))
  return(as.data.frame(out))
}

# beta for SEIRS (default) and anything else (e.g. SIR) 
beta = function(R0,gamma,sigma=0,mu=0,model="seirs") {
  if(model == "seirs") {
    f = (gamma + mu) * (sigma + mu) / sigma
  } else {
    f = gamma
  }
  beta = R0 * f
  return(beta)
}

# vac is the vaccination fraction - it is removed from S but not added to R
sir = function(beta=3, gamma=2, vac=0, tmax=20, step=sir_system_time_step) {
  time  = seq(0, tmax, by=step)
  parms = c(mu=0, N=1, beta=beta, gamma=gamma)
  start = c(S=1-sir_init_i-vac, I=sir_init_i, R=0)
  out   = ode(y     = start, 
              times = time, 
              func  = sir_system, 
              parms = parms)
  out   = cbind(out,data.frame(R0=beta/gamma,vac=vac))
  return(as.data.frame(out))
}
