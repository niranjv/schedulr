rm(list=ls())

# read file for instance type
instance.type <- 'm3xlarge'
filepath <- file.path('inst', 'extdata', paste(instance.type, '_runtimes.csv', sep=''))
sig <- read.csv(filepath, header=T, row.names=1)

# get uniq sizes
uniq.sizes <-  sort(unique(sig[,1]))
mean.runtimes <- matrix(nrow=length(uniq.sizes), ncol=2)
colnames(mean.runtimes) <- c('size_bp', 'mean_runtime_sec')

# get mean runtimes for each size 
for(i in 1:length(uniq.sizes)) {
	s <- uniq.sizes[i]
	size.subset <- subset(sig, sig[,1] == s)
	m <- round(mean(size.subset[,2])) # approx mean is OK
	
	mean.runtimes[i,1] <- s
	mean.runtimes[i,2] <- m
} # end for - loop over all unique region sizes

# plot mean runtimes by size
plot(mean.runtimes, main='Mean runtimes by region size')
lines(lowess(mean.runtimes[,1], mean.runtimes[,2]), col='red', lwd=2)
model <- lm(mean.runtimes[,2] ~ mean.runtimes[,1])
lines(mean.runtimes[,1], model$fitted.values, col='blue', lwd=2)

# save mean runtimes to CSV file
filepath <- file.path('inst', 'extdata', paste(instance.type, '_mean_runtimes.csv', sep=''))
write.csv(mean.runtimes, file=filepath, row.names=F, quote=F)
