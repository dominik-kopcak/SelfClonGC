library(vcfR, quietly=TRUE)
library(dplyr, quietly=TRUE)

setwd("./Simulation_outputs")

h_values = c(0.2)
N_h_values = length(h_values)

s_values = c(0.01)
N_s_values = length(s_values)

gamma_values = c(10**seq(-5, -11, -1))
N_gamma_values = length(gamma_values)

lambda_values = c(4000)
N_lambda_values = length(lambda_values)

N_replicates = 100

#Fixed_mutations_list_GC <- list.files(pattern="X_Fixed_mutations*")
Segregating_mutations_list_GC <-list.files(pattern="X_Sampled_individuals*")

#print(gamma_values)
#print(Fixed_mutations_list_GC)

### Extracting the number of fixed mutations from files to vectors
#N_fixed_mutations_GC <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_gamma_values)

#i_c=0
#for (x in Fixed_mutations_list_GC){
#	i_c = i_c + 1
#	file_c <- read.csv(x, skip=1) 
#	N_fixed_mutations_GC[i_c] <- nrow(file_c)}
	
### Extracting the number of segregeting mutations from files to vectors

N_seg_homo_genotypes_GC <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_gamma_values*N_lambda_values)
N_segregating_mutations_GC <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_gamma_values*N_lambda_values)

j_c = 0
for (y in Segregating_mutations_list_GC){ 
	j_c = j_c + 1
	file_s <- read.vcfR(y, verbose = FALSE) 
	genotype <- extract_gt_tidy(file_s, verbose = FALSE)$gt_GT 
	
	genotype_number <- rep(c(0),times=length(genotype)) 
	
	k=0
	for (z in genotype){ 
		k = k + 1
		allele_1 <- as.numeric(substring(z,1,1))
		allele_2 <- as.numeric(substring(z,3,3))
		genotype_number[k] = allele_1 + allele_2}
	
	N_seg_homo_genotypes_GC[j_c] <- length(genotype_number[genotype_number == 2]) ## recessive load
	N_segregating_mutations_GC[j_c] <- sum(genotype_number) ## additive load
}

# following the naming convetion of the SLiM output file

Selection_coefficient <- rep(s_values, each = N_h_values*N_lambda_values*N_gamma_values*N_replicates)
Dominance_coefficient <- rep(rep(h_values, each = N_gamma_values*N_lambda_values*N_replicates), times = N_s_values)
GC_rate <- rep(rep(gamma_values, each=N_lambda_values*N_replicates), times = N_s_values*N_h_values)
Mean_tract_length <- rep(rep(lambda_values, each=N_replicates), times = N_gamma_values*N_s_values*N_h_values)
Replicate <- rep(c(1:N_replicates), times = N_gamma_values*N_lambda_values*N_s_values*N_h_values)

#Fixed_mutations_GC <- data.frame(Selection_coefficient, Dominance_coefficient, GC_rate, Mean_tract_length, Replicate, N_fixed_mutations_GC)

Recessive_load_GC <- data.frame(Selection_coefficient, Dominance_coefficient, GC_rate, Mean_tract_length, Replicate, N_seg_homo_genotypes_GC)
Additive_load_GC <- data.frame(Selection_coefficient, Dominance_coefficient, GC_rate, Mean_tract_length, Replicate, N_segregating_mutations_GC)


### Summarize and save data into files

#Fixed_mutations_stat_GC <- Fixed_mutations_GC %>%
#	group_by(Selection_coefficient, Dominance_coefficient, GC_rate) %>%
#	summarise(Expected_N_fixed_mutations=mean(N_fixed_mutations_GC), SD_N_fixed_mutations=sd(N_fixed_mutations_GC), .groups = "rowwise")

Recessive_load_stat_GC <- Recessive_load_GC %>%
	group_by(Selection_coefficient, Dominance_coefficient, GC_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_GC), SD_recessive_load=sd(N_seg_homo_genotypes_GC), .groups = "rowwise")

Additive_load_stat_GC <- Additive_load_GC %>%
	group_by(Selection_coefficient, Dominance_coefficient, GC_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_GC), SD_additive_load=sd(N_segregating_mutations_GC), .groups = "rowwise")

setwd("../")

#write.csv(Fixed_mutations_stat_GC, file="Data_GC_fixed_mutations_vXII_full_60.csv")
write.csv(Additive_load_stat_GC, file="Data_GC_additive_load_vXII_full_60.csv")
write.csv(Recessive_load_stat_GC, file="Data_GC_recessive_load_vXII_full_60.csv")
write.csv(Additive_load_GC, file="Data_GC_additive_load_vXII_full_60_all.csv")
write.csv(Recessive_load_GC, file="Data_GC_recessive_load_vXII_full_60_all.csv")
