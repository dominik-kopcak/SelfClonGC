#!/usr/bin/env Rscript
setwd("./Simulation_output")

library(vcfR)
library(reshape2)
library(tidyverse)
library(inbreedR)

vcf_file_list <- list.files(pattern="*.vcf")
vcf_file_list <- vcf_file_list[!grepl("*.vcf.[:hap, log:]", vcf_file_list)]
vcf_file_list_cloning <- vcf_file_list[grepl("cloning", vcf_file_list)]

print(head(vcf_file_list_cloning))

prepare_data <- function(vcf_file){

	# read vcf
	vcf <- read.vcfR(vcf_file, verbose = FALSE )
	# extract genotypes
	gt <- extract.gt(vcf)
	# transpose and data.frame
	gt <- as.data.frame(t(gt), stringsAsFactors = FALSE)
	# split columns and remove middle | symbol
	split_genotype <- function(x){
		colsplit(x, "", c("a","b","c"))[-2]
		}
	snp_geno <- do.call(cbind, apply(gt, 2, split_genotype))
	# convert
	prepared_data <- inbreedR::convert_raw(snp_geno)
	# check data
	check_data(prepared_data)
	
	return(prepared_data)
}

print("--")

print(length(vcf_file_list_cloning))

g2_cloning_vec <- c()
CI_lower_cloning_vec <- c()
CI_upper_cloning_vec <- c()
Replicates_cloning_vec <- c()
Rate_cloning_vec <- c()


## Calculate ID using inbreedR

for(vcf_file in vcf_file_list_cloning){

	print(vcf_file)

	g2_res <- g2_snps(prepare_data(vcf_file), nperm = 0, nboot = 100, CI = 0.95, parallel = TRUE, ncores = 8)                         
	g2_cloning_vec <- append(g2_res$g2, g2_cloning_vec) 
	CI_lower_cloning_vec <- append(g2_res$CI_boot[1], CI_lower_cloning_vec)   
	CI_upper_cloning_vec <- append(g2_res$CI_boot[2], CI_upper_cloning_vec)
	Replicates_cloning_vec <- append(gsub("replicate", "", strsplit(vcf_file, split = "_")[[1]][7]), Replicates_cloning_vec)
	Rate_cloning_vec <- append(as.numeric(gsub("cloning", "", strsplit(vcf_file, split = "_")[[1]][6])), Rate_cloning_vec)
}

	

ID_df_cloning <- data.frame(g2 = g2_cloning_vec,
			    CI_lower = CI_lower_cloning_vec,
		            CI_upper = CI_upper_cloning_vec,
			    Rate = Rate_cloning_vec,
			    Rep = Replicates_cloning_vec)
rownames(ID_df_cloning) <- NULL

# Plotting ID decay (g_2)
setwd("./..")

write.csv(ID_df_cloning, file="Data_ID_cloning.csv")

print(warnings())
