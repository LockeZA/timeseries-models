library(expm)

hmm.state_decode = function(x, hmm, delta.curr=NULL){
  #x - vector of observations
  #hmm - list of paramters returned by the function hmm.fit()
  #delta - optional starting distribution
  #seperate state paramaters and transition probability matrix
  state.params = hmm$state.params
  tpm = hmm$tpm
  #caluclate P(x)
  P = do.call(lapply, c(list(X=x, FUN=hmm.P, dens=hmm$dens), c(state.params)))
  N = length(x)
  #initialize matrix to contain deltas
  states = matrix(NA, nrow=N, ncol=hmm$nstates)
  #if no delta was given, set initial delta to stationary dist
  if (is.null(delta.curr)){
    delta.curr = solve(t(diag(nrow(tpm))-tpm+1),rep(1,nrow(tpm)))
  }
  
  for (i in 1:N){
    d = delta.curr%*%tpm%*%P[[i]]
    states[i, ] = d/sum(d)
    delta.curr = states[i, ]
  }
  return(states)
}

hmm.state_argmax = function(deltas){
  #Returns which state is most likely for each delta
  return(apply(deltas, 1, which.max))
}

hmm.conditional = function(x, delta, t, hmm){
  state.params =hmm$state.params
  tpm = hmm$tpm
  #caluclate P(x)
  P = do.call(hmm.P, c(list(x=x, dens=hmm$dens), c(state.params)))
  #require library(expm) for %^% operator
  return(delta%*%(tpm%^%t)%*%diag(P))
}

hmm.predict = function(delta, t, hmm, ...){
  state.params = hmm$state.params
  tpm = hmm$tpm
  mean.fn = hmm$mean.fn
  var.fn = hmm$var.fn
  #state distribution in t time steps
  delta_t = delta%*%(tpm%^%t)
  Es = do.call(mean.fn, c(state.params, list(...)))
  E = delta_t%*%Es
  Vars = do.call(var.fn, c(state.params, list(...)))
  Var = delta_t^2%*%Vars
  sd = sqrt(Var)
  return(E)
}

hmm.forecast = function(y, t, hmm, ...){
  states = hmm.state_decode(x=y, hmm=hmm, ...)
  y.hat = unlist(apply(states, 1, hmm.predict, t=t, hmm=hmm, ...))
  return(list(y.hat=y.hat, delta=states[nrow(states),]))
}


