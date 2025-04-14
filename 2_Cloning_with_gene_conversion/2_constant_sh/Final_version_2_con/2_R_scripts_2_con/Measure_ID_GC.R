#!/usr/bin/env Rscript
setwd("./Simulation_output")

library(vcfR)
library(reshape2)
library(tidyverse)
library(inbreedR)

vcf_file_list <- list.files(pattern="*.vcf")
vcf_file_list_GC <- vcf_file_list[!grepl("*.vcf.[:hap, log:]", vcf_file_list)]

print(head(vcf_file_list_GC))

prepare_data <- function(vcf_file){

	# read vcf
	vcf <- read.vcfR(vcf_file, verbose = FALSE )
	# extract genotypes
	gt <- extract.gt(vcf)
	# transpose and data.frame
	gt <- as.data.frame(t(gt), stringsAsFactors = FALSE)
	# split columns
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

print(length(vcf_file_list_GC))

g2_GC_vec <- c()
CI_lower_GC_vec <- c()
CI_upper_GC_vec <- c()
Replicates_GC_vec <- c()
Rate_GC_vec <- c()


for(vcf_file in vcf_file_list_GC){

	print(vcf_file)

	g2_res <- g2_snps(prepare_data(vcf_file), nperm = 0, nboot = 100, CI = 0.95, parallel = TRUE, ncores = 8)                         
	g2_GC_vec <- append(g2_res$g2, g2_GC_vec) 
	CI_lower_GC_vec <- append(g2_res$CI_boot[1], CI_lower_GC_vec)   
	CI_upper_GC_vec <- append(g2_res$CI_boot[2], CI_upper_GC_vec)
	Replicates_GC_vec <- append(gsub("replicate", "", strsplit(vcf_file, split = "_")[[1]][8]), Replicates_GC_vec)
	Rate_GC_vec <- append(as.numeric(gsub("gamma", "", strsplit(vcf_file, split = "_")[[1]][6])), Rate_GC_vec)
}

	

ID_df_GC <- data.frame(g2 = g2_GC_vec,
			    CI_lower = CI_lower_GC_vec,
		            CI_upper = CI_upper_GC_vec,
			    Rate = Rate_GC_vec,
			    Rep = Replicates_GC_vec)
rownames(ID_df_GC) <- NULL

# Plotting ID decay (g^2)
setwd("./..")

write.csv(ID_df_GC, file="Data_ID_GC.csv")

print(warnings())
