#!/bin/sh

# 15th August 2023
# Modification of Gertjan Bisschop's script for running array job on server
# SLiM code of polygenic selection model

# Need to first run 'conda activate ##############################################################################################################'

#$ -N SelfClon
#$ -V
#$ -cwd
#$ -t 1-60		# Run command for each line of parameter file
#$ -l h=c4		# Run array job on this sub-server
#$ -o /data/hartfield/asex_load/scripts/output/1_Selfing-cloning_comparison
#$ -e /data/hartfield/asex_load/scripts/error/1_Selfing-cloning_comparison

# Running simulations, parameters in 'parameters_all_1.txt'
SEL=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/constant_sh/parameters_partial_1.txt | awk '{print $1}')
DOM=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/constant_sh/parameters_partial_1.txt | awk '{print $2}')
CLON=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/constant_sh/parameters_partial_1.txt | awk '{print $3}')
REP=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/constant_sh/parameters_partial_1.txt | awk '{print $4}')

NAME_cloning=s${SEL}_h${DOM}_cloning${CLON}_Replicate${REP}_time60000

slim -d s_coeff=$SEL -d h_coeff=$DOM -d Clon_rate=$CLON -d "File_path_i='/scratch/dkopcak/1_Selfing-cloning_comparison/X_Sampled_individuals_$NAME_cloning.vcf'" -d "File_path_f='/scratch/dkopcak/1_Selfing-cloning_comparison/X_Fixed_mutations_$NAME_cloning.csv'" -d "Fitness_mean_file_name='/scratch/dkopcak/1_Selfing-cloning_comparison/Wm_Mean_fitness_$NAME_cloning.csv'" -d "Fitness_variance_file_name='/scratch/dkopcak/1_Selfing-cloning_comparison/Wv_Variance_fitness_$NAME_cloning.csv'" Mutation_accumulation_cloning.slim

