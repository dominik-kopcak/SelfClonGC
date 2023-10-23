#!/bin/sh

# 19th September 2023
# Modification of Gertjan Bisschop's script for running array job on server
# SLiM code of polygenic selection model

# Need to first run 'conda activate ##############################################################################################################'

#$ -N SelfClon
#$ -V
#$ -cwd
#$ -t 1-800		# Run command for each line of parameter file
#$ -l h=c4|c5|c6		# Run array job on this sub-server
#$ -o /data/hartfield/asex_load/scripts/output/1_Selfing-cloning_comparison/nonconstant_sh
#$ -e /data/hartfield/asex_load/scripts/error/1_Selfing-cloning_comparison/nonconstant_sh

# Running simulations, parameters in 'parameters_all_2.txt'
SELF=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/nonconstant_sh/parameters_all_2.txt | awk '{print $1}')
CLON=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/nonconstant_sh/parameters_all_2.txt | awk '{print $1}')
REP=$(sed -n ${SGE_TASK_ID}p /data/hartfield/asex_load/scripts/1_Selfing-cloning_comparison/nonconstant_sh/parameters_all_2.txt | awk '{print $2}')

NAME_selfing=selfing${SELF}_replicate${REP}
NAME_cloning=cloning${CLON}_Replicate${REP}


slim -d Self_rate=$SELF -d "File_path_i='/scratch/dkopcak/1_Selfing-cloning_comparison/X_Sampled_individuals_$NAME_selfing.vcf'" -d "File_path_f='/scratch/dkopcak/1_Selfing-cloning_comparison/X_Fixed_mutations_$NAME_selfing.csv'" -d "Fitness_mean_file_name='/scratch/dkopcak/1_Selfing-cloning_comparison/Wm_Mean_fitness_$NAME_selfing.csv'" -d "Fitness_variance_file_name='/scratch/dkopcak/1_Selfing-cloning_comparison/Wv_Variance_fitness_$NAME_selfing.csv'" Mutation_accumulation_selfing_nonconstant_sh.slim 

slim -d Clon_rate=$CLON -d "File_path_i='/scratch/dkopcak/1_Selfing-cloning_comparison/X_Sampled_individuals_$NAME_cloning.vcf'" -d "File_path_f='/scratch/dkopcak/1_Selfing-cloning_comparison/X_Fixed_mutations_$NAME_cloning.csv'" -d "Fitness_mean_file_name='/scratch/dkopcak/1_Selfing-cloning_comparison/Wm_Mean_fitness_$NAME_cloning.csv'" -d "Fitness_variance_file_name='/scratch/dkopcak/1_Selfing-cloning_comparison/Wv_Variance_fitness_$NAME_cloning.csv'" Mutation_accumulation_nonconstant_sh.slim

