##primetest.r
###use randomization inference to look for changes in responses induced by experiment in subsets by covariates.
####RdE September 2012

#####recodes
####race variable
prime_data$non.white = ifelse(prime_data$race_4 == 1, 1,0)
prime_data$non.white[is.na(prime_data$non.white)==T] = 0
prime_data$residency.new[prime_data$residency == 1] = 1
prime_data$residency.new[prime_data$residency == 2] = 3.5
prime_data$residency.new[prime_data$residency == 3] = 7.5
prime_data$residency.new[prime_data$residency == 4] = 12.5
prime_data$residency.new[prime_data$residency == 5] = mean(prime_data$age, na.rm = T)-15
prime_data$residency.new[prime_data$residency == 6] = mean(prime_data$age, na.rm = T)

#####recodes
###English language is reverse coded from other variables:
prime_data$Englishlan.x = recode(prime_data$Englishlan.x, 
                                 "5" = "1", "4" = "2", "2" = "4", "1" = "5")
prime_data$Englishlan.y = recode(prime_data$Englishlan.y, 
                                 "5" = "1", "4" = "2", "2" = "4", "1" = "5")

###gender recode
prime_data$male = ifelse(prime_data$gender == 1, 1, 0)

###inference
repeats = c("numberim","Remain","Englishlan")

x.names = paste(repeats,".x",sep="")
y.names = paste(repeats,".y",sep="")

covariates = c('line')
final.mat = matrix(nrow = 0, ncol = 8)
subsets = c('all.prime')

for(subset in subsets){ ##b.only, complier, and non-compler subsets
	out.mat = matrix(nrow = length(repeats), ncol = 8)
	dat.subset = prime_data
		
	z.variable = 'treatment'
	
	for(j in 1:length(repeats)){
		dat.subset$x.new = (as.numeric(dat.subset[,x.names[j]])-1)/4  ##rescale x to 0-1
		dat.subset$y.new = (as.numeric(dat.subset[,y.names[j]])-1)/4  ##rescale y to 0-1
		dat.subset$Y = dat.subset$y.new - dat.subset$x.new
			
		dat.use = dat.subset[is.na(dat.subset$Y) == F,]
			
		x.sd = sd(dat.use$x.new,na.rm = T)
		x.mean = mean(dat.use$x.new,na.rm = T)
		
		Xs = data.matrix(dat.use[,covariates])
		
		perms <- genperms(Z = dat.use[,z.variable])
		probs = genprobexact(Z = dat.use[,z.variable])
		ate = estate(Y = dat.use$Y, Z = dat.use[,z.variable], X = Xs, prob = probs)
	
		Ys = genouts(Y = dat.use$Y, Z = dat.use[,z.variable], ate = 0)
		distout <- gendist(Ys,perms, prob=probs)
		disp =	dispdist(distout, ate = ate, display.plot = F)
		
		##fill matrix
		out.mat[j,1] = repeats[j]
		out.mat[j,2] = subset
		out.mat[j,3] = nrow(dat.use)
		out.mat[j,4] = ate
		out.mat[j,5] = disp$greater.p.value
		out.mat[j,6] = disp$lesser.p.value
		out.mat[j,7] = x.sd
		out.mat[j,8] = x.mean
		}
		final.mat = rbind(final.mat,out.mat)
		}
	final.mat = as.data.frame(final.mat)
	colnames(final.mat) = c('variable','subset','N','ate','greater.p.value','lesser.p.value','x.sd','x.mean')
	
	final.mat.prime = final.mat ##mat for creating output later


