library(dplyr)

setwd("./Simulation_outputs")

h_values = c(0.2, 0.5)
N_h_values = length(h_values)

s_values = c(0.01)
N_s_values = length(s_values)

gamma_values = c(10**seq(-5, -13, -1))
N_gamma_values = length(gamma_values)

lambda_values = c(100)
N_lambda_values = length(lambda_values)

N_replicates = 3
Max_time=20000

Mean_fitness_list_GC <- list.files(pattern="Wm_")
Variance_fitness_list_GC <- list.files(pattern="Wv_")

#print(gamma_values)
#print(Mean_fitness_list_GC)

Selection_coefficient <- rep(s_values, each = N_h_values*N_gamma_values*N_lambda_values*Max_time*N_replicates)
Dominance_coefficient <- rep(rep(h_values, each = N_gamma_values*N_lambda_values*Max_time*N_replicates), times = N_s_values)
GC_rate <- rep(rep(gamma_values, each = N_lambda_values*Max_time*N_replicates), times = N_s_values*N_h_values)
Mean_tract_length <- rep(rep(lambda_values, each = Max_time*N_replicates), times = N_s_values*N_h_values*N_gamma_values)
Replicate <- rep(rep(c(1:N_replicates), each = Max_time), times = N_s_values*N_h_values*N_gamma_values*N_lambda_values)
Time <- rep(c(1:Max_time), times = N_s_values*N_h_values*N_gamma_values*N_lambda_values*N_replicates)

Mean_fitness_GC <-rep(c(0), each=N_s_values*N_h_values*N_gamma_values*N_lambda_values*Max_time*N_replicates)

Variance_fitness_GC <-rep(c(0), each=N_s_values*N_h_values*N_gamma_values*N_lambda_values*Max_time*N_replicates)

i=0
for (w_list in Mean_fitness_list_GC){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1){
		#print(w)
		j=j+1
		#print(i*Max_time+j)
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

Fitness_GC <- data.frame(Selection_coefficient, Dominance_coefficient, GC_rate, Mean_tract_length, Replicate, Time, Mean_fitness_GC, Variance_fitness_GC)


Fitness_GC_stat <- Fitness_GC %>%
	group_by(Selection_coefficient, Dominance_coefficient, GC_rate, Time) %>%
	summarize(Expected_mean_fitness = mean(Mean_fitness_GC), Expected_variance_fitness = mean(Variance_fitness_GC),
		  SD_mean_fitness = sd(Mean_fitness_GC), SD_variance_fitness = sd(Variance_fitness_GC), .groups = "rowwise")

setwd("../")

write.csv(Fitness_GC_stat, file="Data_fitness_evolution_GC_vXII_test.csv")
