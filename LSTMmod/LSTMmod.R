init.lstm.mod = function(input.size, hidden.size, delta.hidden.size ,delta.size){
  #calculate number of weights at each gate
  n.weights = (input.size + hidden.size)*hidden.size
  lstm = list(
    a.W = matrix(rnorm(n.weights, sd=0.01), ncol=hidden.size),
    a.b = matrix(rnorm(hidden.size, sd=0.01), ncol=hidden.size),
    
    i.W = matrix(rnorm(n.weights, sd=0.01), ncol=hidden.size),
    i.b = matrix(rnorm(hidden.size, sd=0.01), ncol=hidden.size),
    
    f.W = matrix(rnorm(n.weights, sd=0.01), ncol=hidden.size),
    f.b = matrix(rnorm(hidden.size, sd=0.01), ncol=hidden.size),
    
    o.W = matrix(rnorm(n.weights, sd=0.01), ncol=hidden.size),
    o.b = matrix(rnorm(hidden.size, sd=0.01), ncol=hidden.size),
    
    d.h.W = matrix(rnorm(hidden.size*delta.hidden.size, sd=0.01), nrow=hidden.size),
    d.h.b = matrix(rnorm(delta.hidden.size, sd=0.01), ncol=delta.hidden.size),
    
    d.W = matrix(rnorm(delta.hidden.size*delta.size, sd=0.01), nrow=delta.hidden.size),
    d.b = matrix(rnorm(delta.size, sd=0.01), ncol=delta.size),
    
    mu = rep(0, delta.size),
    v = rep(2, delta.size),
    k = rep(3, delta.size)
  )
  return(lstm)
}

mod.forward.pass = function(x, lstm, h=NULL, C=NULL){
  
  if (is.null(h)){
    h = matrix(0, ncol=ncol(lstm$f.W))
  }
  if (is.null(C)){
    C = matrix(0, ncol=ncol(lstm$f.W))
  }
  xh = cbind(x, h)
  #calculate value at forget gate
  f = sigmoid(xh%*%lstm$f.W+lstm$f.b)
  #calculate value at input gate
  i = sigmoid(xh%*%lstm$i.W+lstm$i.b)
  #calculate input activation
  a = tanh(xh%*%lstm$a.W+lstm$a.b)
  #update the internal cell state
  C = f*C + i*a
  #calculate value at output gate
  o = sigmoid(xh%*%lstm$o.W+lstm$o.b)
  #update hidden state
  h = o*tanh(C)
  #get hidden layer vector
  dh = sigmoid(h%*%lstm$d.h.W + lstm$d.h.b)
  #get output layer
  delta = softmax(dh%*%lstm$d.W + lstm$d.b)
  #get prediction
  yhat = delta%*%lstm$mu
  
  return(list(yhat=yhat, delta=delta, h=h, C=C))
}

lstm = init.lstm.mod(1, 1, 4, 3)
x = rnorm(1)
mod.forward.pass(x, lstm)






