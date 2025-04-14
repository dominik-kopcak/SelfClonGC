#!/usr/bin/env Rscript
setwd("./Simulation_output")

library(vcfR)
library(reshape2)
library(tidyverse)
library(inbreedR)

vcf_file_list <- list.files(pattern="*.vcf")
vcf_file_list <- vcf_file_list[!grepl("*.vcf.[:hap, log:]", vcf_file_list)]
vcf_file_list_selfing <- vcf_file_list[grepl("selfing", vcf_file_list)]
#vcf_file_list_selfing <- vcf_file_list_selfing[grepl("replicate100", vcf_file_list_selfing)]

print(head(vcf_file_list_selfing))

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

	print(length(vcf_file_list_selfing))

	g2_selfing_vec <- c()
	CI_lower_selfing_vec <- c()
	CI_upper_selfing_vec <- c()
	Replicates_selfing_vec <- c()
	Rate_selfing_vec <- c()


	for(vcf_file in vcf_file_list_selfing){
	
		print(vcf_file)

		g2_res <- g2_snps(prepare_data(vcf_file), nperm = 0, nboot = 100, CI = 0.95, parallel = TRUE, ncores = 8)                         
		g2_selfing_vec <- append(g2_res$g2, g2_selfing_vec) 
		CI_lower_selfing_vec <- append(g2_res$CI_boot[1], CI_lower_selfing_vec)   
		CI_upper_selfing_vec <- append(g2_res$CI_boot[2], CI_upper_selfing_vec)
		Replicates_selfing_vec <- append(gsub("replicate", "", strsplit(vcf_file, split = "_")[[1]][5]), Replicates_selfing_vec)
		Rate_selfing_vec <- append(as.numeric(gsub("selfing", "", strsplit(vcf_file, split = "_")[[1]][4])), Rate_selfing_vec)
	}
	
	

ID_df_selfing <- data.frame(g2 = g2_selfing_vec,
			    CI_lower = CI_lower_selfing_vec,
		            CI_upper = CI_upper_selfing_vec,
			    Rate = Rate_selfing_vec,
			    Rep = Replicates_selfing_vec)
rownames(ID_df_selfing) <- NULL

# Plotting ID decay (g^2)
setwd("./..")

write.csv(ID_df_selfing, file="Data_ID_nonconstant_selfing.csv")

print(warnings())
