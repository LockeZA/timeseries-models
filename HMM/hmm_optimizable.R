hmm.optimizable = function(parvec, ind, wp2np.fn, x, dens){
  #parvec - single vector containing all the working parameters to be estimated
  #ind - list that is used to split the parameters in parvec into seperate lists
  #wp2np - list of functions to transform the working parameters to natural parameters
  #x - vector of observations
  #dens - density function eg. dnorm, dpois
  #The function is compatible with optim() or nlm() -> It can maximize the log-likelihood
  parlist = listify(parvec, ind) #break up vector of parameters into seperate lists
  parlist = hmm.wp2np(parlist, wp2np.fn) #transform all working parameters to natural parameters
  ll = do.call(hmm.ll,c(list(x=x, dens=dens),c(parlist))) #calculate the log likelihood
  return(-ll)
}

hmm.fit = function(x, parlist, np2wp.fn, wp2np.fn, mean.fn, var.fn, dens){
  parlist = hmm.wp2np(parlist, np2wp.fn)
  parvec_ind = vectorfy(parlist)
  parvec = parvec_ind$parvec
  ind = parvec_ind$ind
  fit = optim(parvec, hmm.optimizable, ind=ind, wp2np.fn=wp2np.fn, x=x, dens=dens, control=list(maxit=500), method="BFGS")
  parvec = fit$par
  parlist = listify(parvec, ind)
  parlist = hmm.wp2np(parlist, wp2np.fn)
  state.params = within(parlist, rm(tpm))
  tpm = parlist$tpm

  return(list(dens=dens, tpm=tpm, state.params=state.params, 
              mean.fn=mean.fn, var.fn=var.fn, 
              nstates=nrow(tpm), convergence=fit$convergence, ll=-fit$value))
}





