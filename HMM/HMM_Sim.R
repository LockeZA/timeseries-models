#define transition probability matrix for simulated data
gamma = matrix(c(0.03, 0.03, 0.94,
               0.94, 0.03, 0.03,
               0.03, 0.94, 0.03), 3, 3, byrow=T)

#define means and standard deviations for the different states
means = c(-5, 0, 5)
sds = c(1, 1, 1)

#initialize variables
set.seed(2)
N = 10000
#vector to store state the process is in
actual.states = numeric(N)
#vector to store observations
observations = numeric(N)
#set first state to S_1
actual.states[1] = 1
#generate first observation
observation[1] = dnorm(1, means[1], sds[1])
#i'th index of delta gives probability of being in state S_i
delta = matrix(c(1, 0, 0), nrow=1, byrow=T)

#generate random uniform values
U = runif(N)

for (i in 2:N){
  delta = delta%*%gamma
  su = 0
  
  for (s in 1:3){
    su = su + delta[s]
    if (U[i] < su){
      actual.states[i]=s
      observations[i] = rnorm(1, means[s], sds[s])
      delta = rep(0, 3)
      delta[s] = 1
      break
    }
  }
}

library(ggplot2)

#plot first 200 points of data
ggplot(data=NULL, aes(x=1:200, y=observations[1:200])) + 
  geom_line() + 
  geom_point(aes(color=factor(actual.states[1:200]))) +
  xlab("t") +
  ylab("Return") +
  guides(color=guide_legend(title="State"))

observations


