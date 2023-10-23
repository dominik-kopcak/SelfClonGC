#!/usr/bin/env Rscript
setwd("./Simulation_outputs")

library(dplyr)
library(ggplot2)

Uniparenting_rates <- c(0.80, 0.95, 0.99, 1.00)
N_uniparenting_rates = length(Uniparenting_rates)

N_replicates = 15
Max_time=60000

Mean_fitness_list <- list.files(pattern="Wm_")
Variance_fitness_list <- list.files(pattern="Wv_")

Mean_fitness_list_cloning <- Mean_fitness_list[grepl("cloning", Mean_fitness_list)]
Variance_fitness_list_cloning <- Variance_fitness_list[grepl("cloning", Variance_fitness_list)]

print(length(Mean_fitness_list_cloning))

Uniparenting_rate <- rep(rep(Uniparenting_rates, each = Max_time*N_replicates))
Replicate <- rep(rep(c(1:N_replicates), each = Max_time), times = N_uniparenting_rates)
Time <- rep(c(1:Max_time), times = N_uniparenting_rates*N_replicates)

Mean_fitness_cloning <-rep(c(0), each=N_uniparenting_rates*Max_time*N_replicates)

Variance_fitness_cloning <-rep(c(0), each=N_uniparenting_rates*Max_time*N_replicates)

i=0
for (w_list in Mean_fitness_list_cloning){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1){
		j=j+1
		Mean_fitness_cloning[i*Max_time+j] <- w	
	}
	i=i+1
}

i=0
for (w_list in Variance_fitness_list_cloning){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1){
		j=j+1
		Variance_fitness_cloning[i*Max_time+j] <- w	
	}
	i=i+1
}

Fitness_cloning <- data.frame(Uniparenting_rate, Replicate, Time, Mean_fitness_cloning, Variance_fitness_cloning)

Fitness_cloning_stat <- Fitness_cloning %>%
	group_by(Uniparenting_rate, Time) %>%
	summarize(Expected_mean_fitness = mean(Mean_fitness_cloning), Expected_variance_fitness = mean(Variance_fitness_cloning),
		  SD_mean_fitness = sd(Mean_fitness_cloning), SD_variance_fitness = sd(Variance_fitness_cloning), .groups = "rowwise")

setwd("../")

write.csv(Fitness_cloning_stat, file="Data_fitness_evolution_cloning_60000.csv")
