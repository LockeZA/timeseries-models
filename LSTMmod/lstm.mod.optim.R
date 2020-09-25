install.packages("optimParallel")
library(optimParallel)



lstm.mod.optimizable = function(parvec, y, X, ind, hidden.size, delta.hidden.size, delta.size){
  lstm = listify(parvec, ind)
  gates = c("a.W", "a.b", "i.W", "i.b", "f.W", "f.b", "o.W", "o.b")
  for (vec in gates){
    lstm[[vec]] = matrix(lstm[[vec]], ncol=hidden.size)
  }
  lstm$d.h.W = matrix(lstm$d.h.W, nrow=hidden.size)
  lstm$d.h.b = matrix(lstm$d.h.b, ncol=delta.hidden.size)
  lstm$d.W = matrix(lstm$d.W, nrow=delta.hidden.size)
  lstm$d.b = matrix(lstm$d.b, ncol=delta.size)
  
  lstm.res = lstm.mod.forward.pass(X, lstm, hidden.size, delta.size)
  
  ll=0
  P = lapply(y, t3.mod, mu=lstm$mu, v=lstm$v, k=lstm$k)
  deltas = lstm.res$deltas
  
  for (i in 1:nrow(deltas)){
    ll = ll + log(deltas[i,]%*%P[[i]])
  }
  return(-ll)
}

lstm.mod.fit = function(y, X, lstm){
  hidden.size = ncol(lstm$a.W)
  delta.hidden.size = ncol(lstm$d.h.b)
  delta.size = ncol(lstm$d.b)
  
  parvecind = vectorfy(lstm)
  parvec=parvecind$parvec
  ind = parvecind$ind
  conv=1
  it = 0
  
  #print("Using CG to find sp for BFGS")
  #fit = optim(parvec, lstm.mod.optimizable, y=y, X=X, ind=ind, 
  #            delta.hidden.size=delta.hidden.size, delta.size=delta.size,
  #            hidden.size=hidden.size, method="CG")
  #parvec=fit$par
  #print(fit$value)
  
  while(conv==1){
    it = it+1
    print(it)
    fit = optim(parvec, lstm.mod.optimizable, y=y, X=X, ind=ind, 
                delta.hidden.size=delta.hidden.size, delta.size=delta.size,
                hidden.size=hidden.size, method="BFGS")
    conv = fit$convergence
    parvec=fit$par
    print(fit$value)
  }
  
  best.lstm = listify(fit$par, ind)
  gates = c("a.W", "a.b", "i.W", "i.b", "f.W", "f.b", "o.W", "o.b")
  for (vec in gates){
    best.lstm[[vec]] = matrix(best.lstm[[vec]], ncol=hidden.size)
  }
  best.lstm$d.h.W = matrix(best.lstm$d.h.W, nrow=hidden.size)
  best.lstm$d.h.b = matrix(best.lstm$d.h.b, ncol=delta.hidden.size)
  best.lstm$d.W = matrix(best.lstm$d.W, nrow=delta.hidden.size)
  best.lstm$d.b = matrix(best.lstm$d.b, ncol=delta.size)
  
  return(list(ll = fit$value, LSTM = best.lstm))
}

lstm.mod.forward.pass = function(X, lstm, hidden.size, delta.size, h=NULL, C=NULL){
  N = nrow(X)
  yhat = numeric(N)
  deltas = matrix(NA, nrow=N, ncol=delta.size)
  hs = matrix(NA, nrow=N, ncol=hidden.size)
  Cs = matrix(NA, nrow=N, ncol=hidden.size)
  ll = 0
  for (i in 1:N){
    yt = mod.forward.pass(X[i,], lstm, h=h, C=C)
    yhat[i] = yt$yhat
    h = yt$h
    C = yt$C
    hs[i,] = h
    Cs[i,] = C
    deltas[i,] = yt$delta
  }
  return(list(yhat=yhat, Cs=Cs, hs=hs, deltas=deltas))
}

t3.mod = function(x, mu, v, k){
  v = exp(v)
  k = exp(k)
  num = gamma((v+1)/2)
  den = k*sqrt(pi*v)*gamma(v/2)*(1+((x-mu)/k)^2/v)^((v+1)/2)
  return(num/den)
}

#y = observations[2:1001]
#X = matrix(observations[1:1000], ncol=1)
#lstm = init.lstm.mod(1, 1, 10, 3)
#lstm = lstm.mod.fit(y, X, lstm)
#fp = lstm.mod.forward.pass(X, lstm, 1, 3)
#lstm = lstm$LSTM
#lstm.res = lstm.mod.forward.pass(X, lstm, 1, 3)
#mean((lstm.res$yhat-y)^2)




