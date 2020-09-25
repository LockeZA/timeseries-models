lstm.optimizable = function(parvec, y, X, ind, hidden.size){
  lstm = listify(parvec, ind)
  gates = c("a.W", "a.b", "i.W", "i.b", "f.W", "f.b", "o.W", "o.b")
  for (vec in gates){
    lstm[[vec]] = matrix(lstm[[vec]], ncol=hidden.size)
  }
  lstm$p.W = matrix(lstm$p.W, nrow=hidden.size)

  lstm.res = lstm.forward.pass(X, lstm, hidden.size)
  yhat = lstm.res$yhat
  
  return(mean((y-yhat)^2))
}

lstm.fit = function(y, X, lstm){
  hidden.size = ncol(lstm$a.W)
  parvecind = vectorfy(lstm)
  parvec=parvecind$parvec
  ind = parvecind$ind
  fit = optim(parvec, lstm.optimizable, y=y, X=X, ind=ind, hidden.size=hidden.size, method="BFGS")
  
  best.lstm = listify(fit$par, ind)
  gates = c("a.W", "a.b", "i.W", "i.b", "f.W", "f.b", "o.W", "o.b")
  for (vec in gates){
    best.lstm[[vec]] = matrix(best.lstm[[vec]], ncol=hidden.size)
  }
  lstm$p.W = matrix(lstm$p.W, nrow=hidden.size)
  return(list(MSE = fit$value, LSTM = best.lstm))
}

lstm.forward.pass = function(X, lstm, hidden.size, h=NULL, C=NULL){
  N = nrow(X)
  yhat = numeric(N)
  hs = matrix(NA, nrow=N, ncol=hidden.size)
  Cs = matrix(NA, nrow=N, ncol=hidden.size)
  for (i in 1:N){
    yt = forward.pass(X[i,], lstm, h=h, C=C)
    yhat[i] = yt$yhat
    h = yt$h
    C = yt$C
    hs[i,] = h
    Cs[i,] = C
  }
  return(list(yhat=yhat, Cs=Cs, hs=hs))
}

#
#lstm = init.lstm(1,3)
#X = matrix(runif(10), ncol=1)
#y = matrix(runif(10), ncol=1)
#lstm.forward.pass(y, X, lstm, 3)
#parvecind = vectorfy(lstm)
#parvec=parvecind$parvec
#ind = parvecind$ind
#lstm.pre = lstm.optimizable(parvec, c(1,1), c(1,1), ind, hidden.size=3)
#lstm$a.W
#lstm.pre$a.W

