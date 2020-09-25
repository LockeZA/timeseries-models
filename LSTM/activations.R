sigmoid = function(x){
  return(1/(1+exp(-x)))
}

tanh = function(x){
  return((exp(x) - exp(-x))/(exp(x) + exp(-x)))
}

softmax = function(x){
  return(exp(x)/sum(exp(x)))
}


