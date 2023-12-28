#!/bin/sh

# 16th December 2023
# Modification of Gertjan Bisschop's script for running array job on server

# Need to first run 'conda activate ##############################################################################################################'

#$ -N ClonGC_XII_test
#$ -V
#$ -cwd
#$ -t 1-60		# Run command for each line of parameter file
#$ -l h=c5		# Run array job on this sub-server
#$ -o /data/hartfield/asex_load/scripts/output/2_Cloning_with_gene_conversion/XII_test
#$ -e /data/hartfield/asex_load/scripts/error/2_Cloning_with_gene_conversion/XII_test

# Running simulations, parameters in 'parameters_lambda_100_vXII_prelim.txt'
SEL=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/constant_sh/parameters_lambda_100_vXII_prelim.txt | awk '{print $1}')
DOM=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/constant_sh/parameters_lambda_100_vXII_prelim.txt | awk '{print $2}')
GAMMA=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/constant_sh/parameters_lambda_100_vXII_prelim.txt | awk '{print $3}')
LAMBDA=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/constant_sh/parameters_lambda_100_vXII_prelim.txt | awk '{print $4}')
REP=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/2_Cloning_with_gene_conversion/constant_sh/parameters_lambda_100_vXII_prelim.txt | awk '{print $5}')

NAME=s${SEL}_h${DOM}_gamma${GAMMA}_lambda${LAMBDA}_replicate${REP}_vXII_test

slim -d s_coeff=$SEL -d h_coeff=$DOM -d GC_rate=$GAMMA -d lambda=$LAMBDA -d "File_path_i='/scratch/dkopcak/2_Cloning_with_gene_conversion/X_Sampled_individuals_$NAME.vcf'" -d "File_path_f='/scratch/dkopcak/2_Cloning_with_gene_conversion/X_Fixed_mutations_$NAME.csv'" -d "Fitness_mean_file_name='/scratch/dkopcak/2_Cloning_with_gene_conversion/Wm_Mean_fitness_$NAME.csv'" -d "Fitness_variance_file_name='/scratch/dkopcak/2_Cloning_with_gene_conversion/Wv_Variance_fitness_$NAME.csv'" Cloning_with_GC_constant_sh_vXII.slim

