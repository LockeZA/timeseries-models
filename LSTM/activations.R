sigmoid = function(x){
  return(1/(1+exp(-x)))
}

tanh = function(x){
  return((exp(x) - exp(-x))/(exp(x) + exp(-x)))
}



