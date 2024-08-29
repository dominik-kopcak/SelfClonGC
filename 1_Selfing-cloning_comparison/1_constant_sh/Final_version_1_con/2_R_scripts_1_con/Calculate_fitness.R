#!/usr/bin/env Rscript
setwd("./Simulation_output")

library(dplyr)
library(ggplot2)

Uniparenting_rates <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.95, 0.99, 1.00)
N_uniparenting_rates = length(Uniparenting_rates)

h_values = c(0.2)
N_h_values = length(h_values)

s_values = c(0.01)
N_s_values = length(s_values)

N_replicates = 100
Max_time=30000

Mean_fitness_list <- list.files(pattern="Wm_")
Variance_fitness_list <- list.files(pattern="Wv_")

Mean_fitness_list_selfing <- Mean_fitness_list[grepl("selfing", Mean_fitness_list)]
Variance_fitness_list_selfing <- Variance_fitness_list[grepl("selfing", Variance_fitness_list)]

Mean_fitness_list_cloning <- Mean_fitness_list[grepl("cloning", Mean_fitness_list)]
Variance_fitness_list_cloning <- Variance_fitness_list[grepl("cloning", Variance_fitness_list)]

#print(Mean_fitness_list)
#print(Uniparenting_rates)

Selection_coefficient <- rep(s_values, each = N_h_values*N_uniparenting_rates*Max_time*N_replicates)
Dominance_coefficient <- rep(rep(h_values, each = N_uniparenting_rates*Max_time*N_replicates), times = N_s_values)
Uniparenting_rate <- rep(rep(Uniparenting_rates, each = Max_time*N_replicates), times = N_s_values*N_h_values)
Replicate <- rep(rep(c(1:N_replicates), each = Max_time), times = N_s_values*N_h_values*N_uniparenting_rates)
Time <- rep(c(1:Max_time), times = N_s_values*N_h_values*N_uniparenting_rates*N_replicates)

Mean_fitness_selfing <-rep(c(0), each=N_s_values*N_h_values*N_uniparenting_rates*Max_time*N_replicates)
Mean_fitness_cloning <-rep(c(0), each=N_s_values*N_h_values*N_uniparenting_rates*Max_time*N_replicates)

Variance_fitness_selfing <-rep(c(0), each=N_s_values*N_h_values*N_uniparenting_rates*Max_time*N_replicates)
Variance_fitness_cloning <-rep(c(0), each=N_s_values*N_h_values*N_uniparenting_rates*Max_time*N_replicates)

i=0
for (w_list in Mean_fitness_list_selfing){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1[seq(from=1, to=Max_time*2, by=2)]){
		j=j+1
		Mean_fitness_selfing[i*Max_time+j] <- w
	}
	i=i+1
}

i=0
for (w_list in Mean_fitness_list_cloning){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1[seq(from=1, to=Max_time*2, by=2)]){
		j=j+1
		Mean_fitness_cloning[i*Max_time+j] <- w	
	}
	i=i+1
}

i=0
for (w_list in Variance_fitness_list_selfing){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1[seq(from=1, to=Max_time*2, by=2)]){
		j=j+1
		Variance_fitness_selfing[i*Max_time+j] <- w	
	}
	i=i+1
}

i=0
for (w_list in Variance_fitness_list_cloning){
	file <- read.csv(w_list, header=FALSE, sep="\t")
	j=0
	for (w in file$V1[seq(from=1, to=Max_time*2, by=2)]){
		j=j+1
		Variance_fitness_cloning[i*Max_time+j] <- w	
	}
	i=i+1
}


Fitness_selfing <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, Time, Mean_fitness_selfing, Variance_fitness_selfing)
Fitness_cloning <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, Time, Mean_fitness_cloning, Variance_fitness_cloning)


Fitness_selfing_stat <- Fitness_selfing %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Time) %>%
	summarize(Expected_mean_fitness = mean(Mean_fitness_selfing), Expected_variance_fitness = mean(Variance_fitness_selfing),
		  SD_mean_fitness = sd(Mean_fitness_selfing), SD_variance_fitness = sd(Variance_fitness_selfing), .groups = "rowwise")

Fitness_cloning_stat <- Fitness_cloning %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Time) %>%
	summarize(Expected_mean_fitness = mean(Mean_fitness_cloning), Expected_variance_fitness = mean(Variance_fitness_cloning),
		  SD_mean_fitness = sd(Mean_fitness_cloning), SD_variance_fitness = sd(Variance_fitness_cloning), .groups = "rowwise")


setwd("../")

write.csv(Fitness_selfing_stat, file="Data_fitness_evolution_selfing_full_60000.csv")
write.csv(Fitness_cloning_stat, file="Data_fitness_evolution_cloning_full_60000.csv")
