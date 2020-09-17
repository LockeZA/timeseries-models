#rm(list = ls())
validation_sets = function(Xt,time,fracs = c(0.70,0.20,0.10),window = 0.05)
{
	N            = length(time)
	train_window = seq(1,floor(N*fracs[1]),1)
	slide        = 1:floor(N*window)
	val_window   = floor(N*fracs[1])+slide
	nsteps       = fracs[2]/window
	sets = list()
	sets[[1]] = list(train_window = train_window, val_window = val_window)
	plot(1,1,ylim = c(0,nsteps),xlim = c(0,N), type = 'n', xlab = 'Index',ylab = 'Set',axes = F)
	axis(1)
	axis(2,at = 1:nsteps,nsteps:1)
	abline(v = cumsum(fracs)*N,lty = 2,col = 'magenta', lwd = 2)
	i = 0
	points(rep(nsteps - i,length(train_window))~train_window,col = 'black', pch = 16)
	points(rep(nsteps - i,length(val_window))~val_window,col = 'blue', pch = 16)
	abline(v = c(max(train_window),max(val_window)),lty = 3)
	for(i in 1:(nsteps-1))
	{
		train_window = train_window + max(slide)
		val_window = val_window + max(slide)
	  sets[[i+1]] = list(train_window = train_window, val_window = val_window)
		points(rep(nsteps - i,length(train_window))~train_window,col = 'black', pch = 16)
		points(rep(nsteps - i,length(val_window))~val_window,col = 'blue', pch = 16)
		abline(v = c(max(train_window),max(val_window)),lty = 3)
	}
	train_window = train_window + max(slide)
  test_window  = (max(val_window)+1):N
  test_set = list(train_window = train_window, test_window = test_window)
	points(rep(nsteps - i-1,length(train_window))~train_window,col = 'grey', pch = 16)
  points(rep(nsteps - i-1,length(test_window))~test_window,col = 'red', pch = 16)
  return(list(sets = sets,test_set = test_set,nsteps=nsteps,slide_length = length(slide), train_length = length(train_window),test_length = length(test_window)))
}

N   = 100
Xt  = rnorm(N)
tt  = seq(1,N,1)
res = validation_sets(Xt,tt)
#length(res$sets)

res



