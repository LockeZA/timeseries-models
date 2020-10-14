install.packages("optimParallel")
library(optimParallel)



lstm.mod.optimizable = function(parvec, y, X, ind, hidden.size, delta.hidden.size, delta.size){
  lstm = listify(parvec, ind)
  gates = c("a.W", "a.b", "i.W", "i.b", "f.W", "f.b", "o.W", "o.b")
  for (vec in gates){
    lstm[[vec]] = matrix(lstm[[vec]], ncol=hidden.size)
  }
  lstm$d.W = matrix(lstm$d.W, ncol=delta.size)
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
  delta.size = ncol(lstm$d.b)
  
  parvecind = vectorfy(lstm)
  parvec=parvecind$parvec
  ind = parvecind$ind
  
  conv=4
  it = 0
  
  while(conv>3){
    it = it+1
    print(it)
    fit = nlm(lstm.mod.optimizable, parvec, y=y, X=X, ind=ind, 
                delta.size=delta.size,
                hidden.size=hidden.size)
    
    conv = fit$code
    parvec=fit$estimate
    print(fit$minimum)
    break
  }
  
  best.lstm = listify(fit$estimate, ind)
  gates = c("a.W", "a.b", "i.W", "i.b", "f.W", "f.b", "o.W", "o.b")
  for (vec in gates){
    best.lstm[[vec]] = matrix(best.lstm[[vec]], ncol=hidden.size)
  }
  best.lstm$d.W = matrix(best.lstm$d.W, ncol=delta.size)
  best.lstm$d.b = matrix(best.lstm$d.b, ncol=delta.size)
  
  return(list(ll = fit$value, LSTM = best.lstm))
}

lstm.mod.forward.pass = function(X, lstm, hidden.size, delta.size, h=NULL, C=NULL, d=NULL){
  N = nrow(X)
  yhat = numeric(N)
  deltas = matrix(NA, nrow=N, ncol=delta.size)
  hs = matrix(NA, nrow=N, ncol=hidden.size)
  Cs = matrix(NA, nrow=N, ncol=hidden.size)
  ll = 0
  for (i in 1:N){
    yt = mod.forward.pass(X[i,], lstm, h=h, C=C, d=d)
    yhat[i] = yt$yhat
    h = yt$h
    C = yt$C
    d = yt$delta
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

y = observations[2:101]
X = matrix(scale(observations[1:100]), ncol=1)
lstm = init.lstm.mod(1, 1, 3)
lstm = lstm.mod.fit(y, X, lstm)
fp = lstm.mod.forward.pass(X, lstm, 1, 3)
lstm = lstm$LSTM
lstm.res = lstm.mod.forward.pass(X, lstm, 1, 3)
mean((lstm.res$yhat-y)^2)

lstm

