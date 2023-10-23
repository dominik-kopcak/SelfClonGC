#!/usr/bin/env Rscript
setwd("./Simulation_outputs")

library(dplyr)
library(ggplot2)

gamma_values = c(10**seq(-5, -7, -1))
N_gamma_values = length(gamma_values)

lambda_values = c(100)
N_lambda_values = length(lambda_values)

N_replicates = 5
Max_time = 30000

Mean_fitness_list_GC <- list.files(pattern="Wm_")
Variance_fitness_list_GC <- list.files(pattern="Wv_")

print(length(Mean_fitness_list_GC))

GC_rate <- rep(rep(gamma_values, each = N_lambda_values*Max_time*N_replicates))
Mean_tract_length <- rep(rep(lambda_values, each = Max_time*N_replicates), times = N_gamma_values)
Replicate <- rep(rep(c(1:N_replicates), each = Max_time), times = N_gamma_values*N_lambda_values)
Time <- rep(c(1:Max_time), times = N_gamma_values*N_lambda_values*N_replicates)

Mean_fitness_GC <-rep(c(0), each = N_gamma_values*N_lambda_values*Max_time*N_replicates)
Variance_fitness_GC <-rep(c(0), each = N_gamma_values*N_lambda_values*Max_time*N_replicates)

print(length(Mean_fitness_GC))

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
		j=j+1
		Variance_fitness_GC[i*Max_time+j] <- w
	}
	i=i+1
}
Fitness_GC <- data.frame(GC_rate, Mean_tract_length, Replicate, Time, Mean_fitness_GC, Variance_fitness_GC)

Fitness_GC_stat <- Fitness_GC %>%
	group_by(GC_rate, Time) %>%
	summarize(Expected_mean_fitness = mean(Mean_fitness_GC), Expected_variance_fitness = mean(Variance_fitness_GC),
		  SD_mean_fitness = sd(Mean_fitness_GC), SD_variance_fitness = sd(Variance_fitness_GC), .groups = "rowwise")

setwd("../")

write.csv(Fitness_GC_stat, file="Data_fitness_evolution_GC.csv")
