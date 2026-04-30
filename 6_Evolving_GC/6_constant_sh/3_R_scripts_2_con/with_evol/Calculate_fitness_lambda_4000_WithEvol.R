library(tidyverse)

# Function to calculate bootstrap CIs from data (for use at end of script)
BSCI <- function(x,reps=1000)
{
	dat <- matrix(NA,nrow=reps,ncol=length(x))
	for(i in 1:reps)
	{
		dat[i,] <- sample(x,replace=T)
	}
	mns <- rowMeans(dat)
	return(paste(quantile(mns,c(0.025,0.975))[1],quantile(mns,c(0.025,0.975))[2], sep=","))
}

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/6_Evolving_GC/6_constant_sh/2_outputs")

h_values = c(0.2)
N_h_values = length(h_values)

s_values = c(0.01)
N_s_values = length(s_values)

gamma_values = c(10**c(-10,-5,-7))
N_gamma_values = length(gamma_values)

#print(gamma_values)

lambda_values = c(4000)
N_lambda_values = length(lambda_values)

gc_sd = c(10**c(-7,-8))
N_gc_sd = length(gc_sd)

N_replicates = 20
Max_time=60000

Mean_fitness_list_GC <- list.files(pattern="Wm_")[grep(list.files(pattern="Wm_"),pattern="_noevol",invert=T)]
Variance_fitness_list_GC <- list.files(pattern="Wv_")[grep(list.files(pattern="Wv_"),pattern="_noevol",invert=T)]
Mean_GC_list <- list.files(pattern="GC_")[grep(list.files(pattern="GC_"),pattern="_noevol",invert=T)]

Selection_coefficient <- rep(s_values, each = N_h_values*N_gamma_values*N_lambda_values*N_gc_sd*Max_time*N_replicates)
Dominance_coefficient <- rep(rep(h_values, each = N_gamma_values*N_lambda_values*N_gc_sd*Max_time*N_replicates), times = N_s_values)
GC_rate <- rep(rep(gamma_values, each = N_lambda_values*N_gc_sd*Max_time*N_replicates), times = N_s_values*N_h_values)
Mean_tract_length <- rep(rep(lambda_values, each = N_gc_sd*Max_time*N_replicates), times = N_s_values*N_h_values*N_gamma_values)
GC_var <- rep(rep(gc_sd, each=Max_time*N_replicates), times = N_s_values*N_h_values*N_gamma_values*N_gc_sd)
rep_temp <- c(1:N_replicates) %>% as.character(.) %>% sort %>% as.numeric
Replicate <- rep(rep(rep_temp, each = Max_time), times = N_s_values*N_h_values*N_gamma_values*N_lambda_values*N_gc_sd)
Time <- rep(c(1:Max_time), times = N_s_values*N_h_values*N_gamma_values*N_lambda_values*N_gc_sd*N_replicates)

Mean_fitness_GC <-rep(c(0), each=N_s_values*N_h_values*N_gamma_values*N_lambda_values* N_gc_sd*Max_time*N_replicates)

Variance_fitness_GC <-rep(c(0), each=N_s_values*N_h_values*N_gamma_values*N_lambda_values* N_gc_sd*Max_time*N_replicates)

Mean_GC <-rep(c(0), each=N_s_values*N_h_values*N_gamma_values*N_lambda_values* N_gc_sd*Max_time*N_replicates)

i=0
for (w_list in Mean_fitness_list_GC){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1){
		j=j+1
		Mean_fitness_GC[i*Max_time+j] <- w	
	}
	i=i+1
}

i=0
for (w_list in Variance_fitness_list_GC){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1){
		#print(w)
		j=j+1
		#print(i*Max_time+j)
		Variance_fitness_GC[i*Max_time+j] <- w	
	}
	i=i+1
}

i=0
for (w_list in Mean_GC_list){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1){
		j=j+1
		Mean_GC[i*Max_time+j] <- w*4000	
	}
	i=i+1
}

Fitness_GC <- data.frame(Selection_coefficient, Dominance_coefficient, GC_rate, Mean_tract_length, GC_var, Replicate, Time, Mean_fitness_GC, Variance_fitness_GC, Mean_GC)

Fitness_GC_stat <- Fitness_GC %>%
	group_by(Selection_coefficient, Dominance_coefficient, GC_rate, GC_var, Time) %>%
	summarize(Expected_mean_fitness = mean(Mean_fitness_GC), Expected_variance_fitness = mean(Variance_fitness_GC), Expected_GC = mean(Mean_GC),
		  SD_mean_fitness = sd(Mean_fitness_GC), SD_variance_fitness = sd(Variance_fitness_GC), SD_GC = sd(Mean_GC), 
		 CI_mean_fit = BSCI(Mean_fitness_GC,100), CI_var_fit = BSCI(Variance_fitness_GC,100), CI_GC = BSCI(Mean_GC,100),
		  .groups = "rowwise") %>% separate(CI_mean_fit, into=c('CI_fit_L', 'CI_fit_U'), sep=',',convert=T) %>% separate(CI_var_fit, into=c('CI_var_L', 'CI_var_U'), sep=',',convert=T) %>% separate(CI_GC, into=c('CI_GC_L', 'CI_GC_U'), sep=',',convert=T)

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/6_Evolving_GC/6_constant_sh/4_Data_2_con")


write.csv(Fitness_GC_stat, file="Data_fitness_evolution_GC_WithEvol.csv")
