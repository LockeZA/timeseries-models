init.lstm = function(input.size, hidden.size){
  #calculate number of weights at each gate
  n.weights = (input.size + hidden.size)*hidden.size
  lstm = list(
    a.W = matrix(rnorm(n.weights), ncol=hidden.size),
    a.b = matrix(rnorm(hidden.size), ncol=hidden.size),
    
    i.W = matrix(rnorm(n.weights), ncol=hidden.size),
    i.b = matrix(rnorm(hidden.size), ncol=hidden.size),
    
    f.W = matrix(rnorm(n.weights), ncol=hidden.size),
    f.b = matrix(rnorm(hidden.size), ncol=hidden.size),
    
    o.W = matrix(rnorm(n.weights), ncol=hidden.size),
    o.b = matrix(rnorm(hidden.size), ncol=hidden.size),
    
    p.W = matrix(rnorm(hidden.size), nrow=hidden.size),
    p.b = matrix(rnorm(1), ncol=1)
  )
  return(lstm)
}

forward.pass = function(x, lstm, h=NULL, C=NULL){
  
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
  #get prediction as a linear combination of hidden state
  yhat = h%*%lstm$p.W + lstm$p.b
  
  return(list(yhat=yhat, h=h, C=C))
}



