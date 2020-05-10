# beta for SEIRS (default) and anything else (e.g. SIR) 
get_beta = function(p) {
  if(p$sigma > 0 | p$mu > 0) {
    f = (p$gamma + p$mu + p$alpha) * (p$sigma + p$mu) / p$sigma
  } else {
    f = p$gamma
  }
  beta = p$R0 * f
  return(beta)
}

# omega, mu inputs are in years
# all rates are returned as per day
# all times are in days
get_params = function(R0,ip=0,lp=0,id=0,le=0,al=0,p=0) {
  out = as.list(c(R0    = R0,
                  gamma = ifelse(ip,1/ip,0),
                  sigma = ifelse(lp,1/lp,0),
                  omega = ifelse(id,1/(365*id),0),
                  mu    = ifelse(le,1/(365*le),0),
                  alpha = ifelse(al,1/al,0),
                  p     = p,
                  N     = 1,
                  ip    = ip,
                  lp    = lp,
                  id    = 365*id,
                  le    = 365*le
                  ))
  out$pc     = 1-1/out$R0
  out$beta   = get_beta(out)
  out$A      = (out$omega+out$mu+out$gamma)/( (out$omega+out$mu) * (out$beta-out$gamma-out$mu) )
  out$cfr    = out$alpha/(out$gamma+out$mu+out$alpha)
  out$stars  = stars(out)
  out$period = ieperiod(out)
  return(out)
}

############################################################################
### SIR
# estimate how long the outbreak lasts
sir_t_bound = function(params,tol=sir_init_i/2,tmax=1000,steps=sir_system_steps) {
  # get a quick solution out to long time
  if(params$R0 <= 1 | params$p >= 1-1/params$R0) {
    return(1000)
  }
  df     = sir(params,tmax=tmax,steps=steps)
  df_cut = df[df$I >= tol,]
  return( df_cut[nrow(df_cut),]$time )
}
# vac is the vaccination fraction - it is removed from S but not added to R
sir = function(params,tmax,steps=sir_system_steps) {
  time   = seq(0, tmax, length.out=steps)
  # squash and replace parameters
  params = replace(params,c("mu","beta"),c(0,params$R0*params$gamma))
  start = c(S = params$N-sir_init_i-params$p, 
            I = sir_init_i, 
            R = params$p)
  out   = ode(y     = start, 
              times = time, 
              func  = sir_system, 
              parms = unlist(params))
  out   = cbind(out,data.frame(R0=params$R0,beta=params$beta,p=params$p))
  return(as.data.frame(out))
}
sir_system = function(t, x, parms){
  S = x[1]
  I = x[2]
  R = x[3]
  beta  = parms["beta"]
  mu    = parms["mu"]    
  gamma = parms["gamma"]
  N     = parms["N"]
  dS    = mu*(N-S)  - beta*S*I/N
  dI    = beta*S*I/N - I*(mu + gamma)
  dR    = gamma*I - mu*R
  res   = c(dS,dI,dR)
  list(res)
}

############################################################################
## SEIRS
# estimate how long the outbreak lasts - this will be used as time axis max
seirs_t_bound = function(params,tol=10*sir_init_i,steps=2*sir_system_steps) {
  # get a quick solution out to long time (10 * 1/omega)
  #beta  = beta(R0,gamma,sigma,mu,model="seirs")
  df    = seirs(params,tmax=ceiling(10 * 1/params$omega),steps=steps)
  # Find the last time for which (S-S*)/S* > 0.001 
  stars = stars(params)
  dfok  = df[abs(df$S - stars$S)/stars$S > tol,]
  t     = tail(dfok,n=1)$t
  return(t)
}

# vac is the vaccination fraction - it is removed from S but not added to R
seirs = function(params, tmax, steps=sir_system_steps) {
  #beta    = beta(R0,gamma,sigma,mu,model="seirs")
  time     = seq(0,tmax,length.out=steps)
  #parms = c(p=vac,mu=mu, N=1, beta=beta, gamma=gamma, sigma=sigma, omega=omega, mu=mu)
  start = c(S = params$N - sir_init_i - params$p, 
            E = sir_init_i, 
            I = 0, 
            R = params$p)
  out   = ode(y     = start, 
              times = time, 
              func  = seirs_system, 
              parms = unlist(params))
  out   = cbind(out,data.frame(R0=params$R0,beta=params$beta,p=params$p))
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
  alpha= params["alpha"]
  p    = params["p"]

  dS  = -beta*S*I + omega*R  + mu*(1-p) - mu*S 
  dE  = -sigma*E  + beta*S*I            - mu*E
  dI  = -gamma*I  + sigma*E             - (mu+alpha)*I
  dR  = -omega*R  + gamma*I  + mu*p     - mu*R
  #      ^          ^          ^          ^
  #      outflow    inflow     birth      death
  res = c(dS, dE, dI, dR)
  list(res)
}

stars = function(params) {
  #beta  = beta(R0,gamma,sigma,mu)
  Sstar = 1/params$R0
  Istar = params$mu*(1-Sstar)/(params$beta*Sstar - (params$omega*params$gamma/(params$mu+params$omega)))
  Estar = (params$mu+params$gamma)*Istar/params$sigma
  Rstar = params$gamma*Istar/(params$mu+params$omega)
  return(as.list(c(S=Sstar,E=Estar,I=Istar,R=Rstar)))
}

ieperiod = function(params) {
  #beta  = beta(R0,gamma,sigma,mu)
  #paras = c(mu=mu, beta=beta, sigma=sigma, gamma=gamma, omega=omega, alpha=alpha, p=p)
  stars = stars(params)
  star  = c(stars(params),params)
  names(star)[1:4]=c("S", "E", "I", "R")

  fns=list(
           quote(mu*(1-p-S) - beta*S*I + omega*R    ), 
           quote(beta*S*I   - (mu+sigma)*E          ), 
           quote(sigma*E    - (mu+alpha+gamma)*I    ), 
           quote(gamma*I    - mu*R - omega*R + p*mu )
           )

  aa=as.vector(c(sapply(fns, D, "S"),
                 sapply(fns, D, "E"),
                 sapply(fns, D, "I"),
                 sapply(fns, D, "R")))
  #print(aa)
  JJ=matrix(sapply(aa,eval,star),ncol=4)
  #print(JJ)
  EE=eigen(JJ)$values
  #print(EE)
  WW=which.max(Im(EE))
  #print(WW)
  rp=2*pi/Im(EE[WW])
  return(rp)
}
