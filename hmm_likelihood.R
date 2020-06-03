hmm.P = function(x, dens, ...){
  #x - vector of observations
  #dens - density function eg. dnorm, dpois
  #... - add parmaters for each state, eg. 
  #      if dnorm with 2 states:mean=c(1,2), sd=c(0.5,1)
  #RETURNS a diagonal matrix of probabilities
  p = mapply(dens, x=x,...)
  return(diag(p))
}

hmm.ll = function(sdis, x, tpm, dens, ...){
  #sdis - starting distribution
  #x - vector of observations
  #tpm - transition probability matrix
  #dens - density function eg. dnorm, dpois
  #... - add parmaters specific to density function
  #RETURNS the log-likelihood of x
  
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

#EXAMPLE USAGE OF hmm.p

#single normal
hmm.P(x=1, mean=c(1,2,3,4), sd=c(1,2,3,4), dens=dnorm)
#multiple normal
lapply(1:2, FUN=hmm.P, mean=c(1,2,3,4), sd=c(1,2,3,4), dens=dnorm)

#single poisson
hmm.P(1, lambda=c(1,2), dens=dpois)
#multiple poisson
lapply(1:2, hmm.P, lambda=c(1,2), dens=dpois)


#EXAMPLE USAGE OF hmm.ll
sdis = c(0.6608,0.3392)
tpm = matrix(c(0.934, 0.0660
               ,0.1285, 0.8715), 2, byrow=T)
lambda = c(15.472, 26.125)
x= c(13,14,8,10,16,26,32,27,18,32,36,24,22,23,22,18,25,21,21,14,
     8,11,14,23,18,17,19,20,22,19,13,26,13,14,22,24,21,22,26,21,
     23,24,27,41,31,27,35,26,28,36,39,21,17,22,17,19,15,34,10,15,
     22,18,15,20,15,22,19,16,30,27,29,23,20,16,21,21,25,16,18,15,
     18,14,10,15,8,15,6,11,8,7,18,16,13,12,13,20,15,16,12,18,
     15,16,13,15,16,11,11)

hmm.ll(sdis, x=x, tpm=tpm, dens=dpois, lambda=lambda)



