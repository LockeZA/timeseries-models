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
    i = j+i
  }
  return(par_list)
}

t = vectorfy(list(a=c(1,2,3), b=c(4,5,6,7)))
t
#$parvec
#a1 a2 a3 b1 b2 b3 b4 
#1  2  3  4  5  6  7 

#$ind
#$ind$a
#[1] 3

#$ind$b
#[1] 4

listify(t$parvec, t$ind)
#$a
#a1 a2 a3 
#1  2  3 

#$b
#b1 b2 b3 b4 
#4  5  6  7 
