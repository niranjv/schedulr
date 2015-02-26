rm(list=ls())

# Assume we are running in the schedulr package dir

# Assume mean runtimes for different sizes are given in the file schedulr/inst/extdata/<INSTANCE_TYPE>_mean_runtimes.csv
# This file is generated outside this package
instance.type <- 'm3xlarge'
mean.runtimes.file <- file.path('inst', 'extdata', paste(instance.type, '_mean_runtimes.csv', sep=''))
mean.runtimes <- read.csv(mean.runtimes.file , header=T)

m3xlarge.runtimes.expdist <- matrix(nrow=250*NROW(mean.runtimes), ncol=2)
colnames(m3xlarge.runtimes.expdist) <- c('size_bp', 'runtime_sec')

for(i in 1:NROW(mean.runtimes)) {
	s <- mean.runtimes[i,1]
	m <- mean.runtimes[i,2]
	m3xlarge.runtimes.expdist.cur.size <- round(rexp(250, rate=1/m),2)
	
	row.min <- ((i-1)*250) + 1
	row.max <- i*250
	m3xlarge.runtimes.expdist[row.min:row.max, 1] <- s
	m3xlarge.runtimes.expdist[row.min:row.max, 2] <- m3xlarge.runtimes.expdist.cur.size
	
} # end for - loop over all sizes

# save CSV file to schedulr/inst/extdata
outfile.path <- file.path('inst', 'extdata', 'training_set_exp_dist.csv')
cat('Writing training set data in CSV format to', outfile.path, '\n')
write.csv(m3xlarge.runtimes.expdist, file=outfile.path, quote=F, row.names=F)


# save .Rda file to schedulr/data 
devtools::use_data(m3xlarge.runtimes.expdist, overwrite=T)
