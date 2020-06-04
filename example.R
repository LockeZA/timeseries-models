#EXAMPLE USAGE OF hmm.optimizable

#define the structure of the hidden markov model

#x is the sequence of observations
x= c(13,14,8,10,16,26,32,27,18,32,36,24,22,23,22,18,25,21,21,14,
     8,11,14,23,18,17,19,20,22,19,13,26,13,14,22,24,21,22,26,21,
     23,24,27,41,31,27,35,26,28,36,39,21,17,22,17,19,15,34,10,15,
     22,18,15,20,15,22,19,16,30,27,29,23,20,16,21,21,25,16,18,15,
     18,14,10,15,8,15,6,11,8,7,18,16,13,12,13,20,15,16,12,18,
     15,16,13,15,16,11,11)

#dens is the density function of the hmm
dens = dpois

#parvec is the vector of parameters to be estimated
parvec = c(-1,-1,1,2)

#ind is a list which specifies the what the paramters in parvec are
ind = list(tpm=2, lambda=2) #the first 2 paramters are for the transition probability matrix and the next 2
#                           parameters are for the lambda parameters
#The names used in the list are important; ie. lambda is the name of the variable that dpois takes in

#trans.fn is a list of transformation functions for the paramters
trans.fn = list(tpm = tpm.wp2np, lambda=lambda.wp2np)
#Once again, the names have to correspond to the names in ind. If a variable is not mentioned in trans.fn,
#then it will not be transformed, for example trans.fn = list(tpm = tpm.wp2np) would not transform the lambda parameters.

nlm(hmm.optimizable,parvec, ind=ind, trans.fn=trans.fn, x=x, dens=dens)

#Using the paramter vector given by nlm, we can transform the working paramters back into natural parameters



