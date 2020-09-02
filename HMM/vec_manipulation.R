vectorfy = function(par_list){
  ind = list()
  parvec = unlist(par_list)
  for (nm in names(par_list)){
    ind[[nm]] = length(par_list[[nm]])
  }
  return(list(parvec=parvec, ind=ind))
}

listify = function(parvec, ind){
  par_list = list()
  i = 0
  for (nm in names(ind)){
    j = ind[[nm]]+i
    par_list[[nm]] = parvec[(i+1):j]
    i = j
  }
  return(par_list)
}
