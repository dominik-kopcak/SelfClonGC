#!/bin/sh

# 19th September 2023
# Modification of Gertjan Bisschop's script for running array job on server

# Need to first run 'conda activate ##############################################################################################################'

#$ -N ClonGC_100_nonC_sh
#$ -V
#$ -cwd
#$ -t 1-15		# Run command for each line of parameter file
#$ -l h=c6		# Run array job on this sub-server
#$ -o /data/hartfield/asex_load/scripts/output/2_Cloning_with_gene_conversion/nonconstant_sh
#$ -e /data/hartfield/asex_load/scripts/error/2_Cloning_with_gene_conversion/nonconstant_sh

# Running simulations, parameters in 'parameters_lambda_100_nonC_sh.txt'
GAMMA=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/nonconstant_sh/parameters_lambda_100_nonC_sh.txt | awk '{print $1}')
LAMBDA=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/nonconstant_sh/parameters_lambda_100_nonC_sh.txt | awk '{print $2}')
REP=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/nonconstant_sh/parameters_lambda_100_nonC_sh.txt | awk '{print $3}')

NAME=gamma${GAMMA}_lambda${LAMBDA}_replicate${REP}_nonconstant_sh

slim -d GC_rate=$GAMMA -d lambda=$LAMBDA -d "File_path_i='/scratch/dkopcak/2_Cloning_with_gene_conversion/nonconstant_sh/X_Sampled_individuals_$NAME.vcf'" -d "File_path_f='/scratch/dkopcak/2_Cloning_with_gene_conversion/nonconstant_sh/X_Fixed_mutations_$NAME.csv'" -d "Fitness_mean_file_name='/scratch/dkopcak/2_Cloning_with_gene_conversion/nonconstant_sh/Wm_Mean_fitness_$NAME.csv'" -d "Fitness_variance_file_name='/scratch/dkopcak/2_Cloning_with_gene_conversion/nonconstant_sh/Wv_Variance_fitness_$NAME.csv'" Cloning_with_GC_nonconstant_sh.slim
