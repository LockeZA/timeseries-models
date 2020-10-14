init.lstm.mod = function(input.size, hidden.size ,delta.size){
  #calculate number of weights at each gate
  n.weights = (input.size + hidden.size)*hidden.size
  lstm = list(
    a.W = matrix(rnorm(n.weights, sd=0.1), ncol=hidden.size),
    a.b = matrix(rnorm(hidden.size, sd=0.1), ncol=hidden.size),
    
    i.W = matrix(rnorm(n.weights, sd=0.1), ncol=hidden.size),
    i.b = matrix(rnorm(hidden.size, sd=0.1), ncol=hidden.size),
    
    f.W = matrix(rnorm(n.weights, sd=0.1), ncol=hidden.size),
    f.b = matrix(rnorm(hidden.size, sd=0.1), ncol=hidden.size),
    
    o.W = matrix(rnorm(n.weights, sd=0.1), ncol=hidden.size),
    o.b = matrix(rnorm(hidden.size, sd=0.1), ncol=hidden.size),
    
    d.W = matrix(rnorm((delta.size+hidden.size)*delta.size, sd=0.1), ncol=delta.size),
    d.b = matrix(rnorm(delta.size, sd=0.1), ncol=delta.size),
    
    mu = rnorm(delta.size, sd=2),
    v = rep(3, delta.size),
    k = rep(4, delta.size)
  )
  return(lstm)
}

mod.forward.pass = function(x, lstm, h=NULL, C=NULL, d=NULL){
  
  if (is.null(h)){
    h = matrix(0, ncol=ncol(lstm$f.W))
  }
  if (is.null(C)){
    C = matrix(0, ncol=ncol(lstm$f.W))
  }
  if (is.null(d)){
    d = matrix(1/ncol(lstm$f.W), ncol=ncol(lstm$d.W))
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
  #concat delta and hidden state
  dh = cbind(d, h)
  #get output layer
  delta = softmax(dh%*%lstm$d.W + lstm$d.b)
  #get prediction
  yhat = delta%*%lstm$mu
  
  return(list(yhat=yhat, delta=delta, h=h, C=C))
}

lstm = init.lstm.mod(1, 1, 3)
x = rnorm(1)
mod.forward.pass(x, lstm)






