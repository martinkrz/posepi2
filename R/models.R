
### SIR
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
sir = function(beta=3, gamma=2, vac=0, tmax=20, steps=sir_system_steps) {
  time  = seq(0, tmax, length.out=steps)
  parms = c(mu=0, N=1, beta=beta, gamma=gamma)
  start = c(S=1-sir_init_i-vac, I=sir_init_i, R=0)
  out   = ode(y     = start, 
              times = time, 
              func  = sir_system, 
              parms = parms)
  out   = cbind(out,data.frame(R0=beta/gamma,vac=vac))
  return(as.data.frame(out))
}

## SEIRS
# estimate how long the outbreak lasts - this will be used as time axis max
seirs_t_bound = function(R0,gamma,sigma,omega,mu,vac=0,tol=10*sir_init_i,steps=2*sir_system_steps) {
  # get a quick solution out to long time (10 * 1/omega)
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  df    = seirs(R0,gamma,sigma,omega,mu,tmax=ceiling(10/omega),steps=steps)
  # Find the last time for which (S-S*)/S* > 0.001 
  stars = stars(R0,gamma,sigma,omega,mu)
  dfok  = df[abs(df$S - stars$S)/stars$S > tol,]
  t     = tail(dfok,n=1)$t
  return(t)
}

# vac is the vaccination fraction - it is removed from S but not added to R
seirs = function(R0=3, gamma=1/14, sigma=1/7, omega=1/365, mu=1/(365*76), vac=0, tmax=365*5, steps=sir_system_steps) {
  beta  = beta(R0,gamma,sigma,mu,model="seirs")
  time  = seq(0, tmax, length.out=steps)
  parms = c(p=vac,mu=mu, N=1, beta=beta, gamma=gamma, sigma=sigma, omega=omega, mu=mu)
  start = c(S=1-sir_init_i-vac, E=sir_init_i, I=0, R=vac)
  out   = ode(y     = start, 
              times = time, 
              func  = seirs_system, 
              parms = parms)
  out   = cbind(out,data.frame(R0=R0,beta=beta,vac=vac))
  return(as.data.frame(out))
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
  p    = params["p"]
  
  dS = -beta*S*I + omega*R  + mu*(1-p) - mu*S 
  dE = -sigma*E  + beta*S*I            - mu*E
  dI = -gamma*I  + sigma*E             - mu*I
  dR = -omega*R  + gamma*I  + mu*p     - mu*R
  #     ^          ^          ^        ^
  #     outflow    inflow     birth    death
  res= c(dS, dE, dI, dR)
  list(res)
}



# beta for SEIRS (default) and anything else (e.g. SIR) 
beta = function(R0,gamma,sigma=0,mu=0,model="seirs") {
  if(model == "seirs" | sigma > 0 | mu > 0) {
    f = (gamma + mu) * (sigma + mu) / sigma
  } else {
    f = gamma
  }
  beta = R0 * f
  return(beta)
}


stars = function(R0,gamma,sigma,omega,mu) {
  beta  = beta(R0,gamma,sigma,mu)
  Sstar = 1/R0
  Istar = mu*(1-Sstar)/(beta*Sstar - (omega*gamma/(mu+omega)))
  Estar = (mu+gamma)*Istar/sigma
  Rstar = gamma*Istar/(mu+omega)
  return(as.list(c(S=Sstar,E=Estar,I=Istar,R=Rstar)))
}

ieperiod = function(R0,gamma,sigma,omega,mu) {
  beta  = beta(R0,gamma,sigma,mu)
  #Sstar = 1/R0
  #Istar = mu*(1-Sstar)/(beta*Sstar - (omega*gamma/(mu+omega)))
  #Estar = (mu+gamma)*Istar/sigma
  #Rstar = gamma*Istar/(mu+omega)
  paras = c(mu = mu, beta =  beta, sigma = sigma, gamma = gamma, omega=omega)
  stars = stars(R0,gamma,sigma,omega,mu)
  #star=as.list(c(S=Sstar, E=Estar, I=Istar, R=Rstar, paras))
  star  = c(stars,as.list(paras))
  names(star)[1:4]=c("S", "E", "I", "R")

  fns=list(quote(mu * (1  - S)  - beta * S * I  + omega * R), quote(beta * S * I - (mu + sigma) * E), quote(sigma * E - (mu + gamma) * I), quote(gamma * I - mu * R - omega * R))

  aa=as.vector(c(sapply(fns, D, "S"),
                 sapply(fns, D, "E"),
                 sapply(fns, D, "I"),
                 sapply(fns, D, "R")))

  JJ=matrix(sapply(aa, eval, star), ncol=4)
  EE=eigen(JJ)$values
  WW=which.max(Im(EE))
  rp=2*pi/Im(EE[WW])
  return(rp)
}
