hmm.optimizable = function(parvec, ind, trans.fn, x, dens){
  par_list = listify(parvec, ind) #break up vector of parameters into respective components
  par_list = hmm.wp2np(par_list, trans.fn) #transform all parameters with regards to their respective transformation
  ll = do.call(hmm.ll,c(list(x=x, dens=dens),c(par_list))) #calculate the log likelihood
  return(-ll)
}








