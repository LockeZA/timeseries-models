hmm.P = function(x, dens, ...){
  #x - vector of observations
  #dens - density function eg. dnorm, dpois
  #... - add parmaters for each state, eg. 
  #      if dnorm with 2 states:mean=c(1,2), sd=c(0.5,1)
  #RETURNS a diagonal matrix of probabilities
  p = mapply(dens, x=x,...)
  return(diag(p))
}

hmm.ll = function(x, tpm, dens, ...){
  #x - vector of observations
  #tpm - transition probability matrix
  #dens - density function eg. dnorm, dpois or custom
  #... - add parmaters specific to density function
  #RETURNS the log-likelihood of x
  
  t = try(solve(t(diag(nrow(tpm))-tpm+1),rep(1,nrow(tpm))), silent=TRUE) #stationary distribution
  #if tpm is singular return the lowest possible log likelihood so that optim can contiue converging
  if("try-error" %in% class(t)){
    return(-.Machine$integer.max)
  }
  sdis = solve(t(diag(nrow(tpm))-tpm+1),rep(1,nrow(tpm)))

  P = lapply(x, hmm.P, ..., dens=dens)
  n = length(sdis)
  phi = sdis
  l = 0
  ones = rep(1,n)
  t = length(P)
  
  for (i in 1:t){
    v = phi%*%tpm%*%P[[i]]
    u = v%*%ones
    l = l + log(u)
    phi = v/u[[1]]
  }
  return(l)
}






