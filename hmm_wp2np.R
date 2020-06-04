#function to transform working parameters of transition probability matrix back to natural parameters
tpm.wp2np = function(wp){
  #wp - vector of working parameters
  #RETURNS a transition probability matrix, where sum of each row = 1
  n = length(wp)
  m = (1+(1+4*n)^0.5)/2 #dimensions of matrix
  
  P = matrix(0, nrow=m, ncol=m)
  P[lower.tri(P)] = wp[1:(n/2)]
  P[upper.tri(P)] = wp[(n/2+1):n]
  P = exp(P)
  P = P/apply(P,1,sum)
  return(P)
}

lambda.wp2np = function(wp){
  return(exp(wp))
}

#function to transform natural paramters to working parameters
hmm.wp2np = function(par, trans.fn){
  #par is a list of parameters
  #trans.fn is a list functions to transform parameters in par
  #RETURNS a list of transformed paramters
  for (nm in names(trans.fn)){ #only transform paramters which are referenced in trans.fn
    par[[nm]] = trans.fn[[nm]](par[[nm]])
  }
  return(par)
}

