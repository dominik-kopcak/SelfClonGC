#!/bin/sh

# Running simulations with parameters stored in 'parameters_vXII_full_60.txt'
SEL=$(sed -n ${SGE_TASK_ID}p parameters_vXII_full_60.txt | awk '{print $1}')
DOM=$(sed -n ${SGE_TASK_ID}p parameters_vXII_full_60.txt | awk '{print $2}')
GAMMA=$(sed -n ${SGE_TASK_ID}p parameters_vXII_full_60.txt | awk '{print $3}')
LAMBDA=$(sed -n ${SGE_TASK_ID}p parameters_vXII_full_60.txt | awk '{print $4}')
REP=$(sed -n ${SGE_TASK_ID}p parameters_vXII_full_60.txt | awk '{print $5}')

NAME=s${SEL}_h${DOM}_gamma${GAMMA}_lambda${LAMBDA}_replicate${REP}_vXII_full_60

slim -d s_coeff=$SEL -d h_coeff=$DOM -d GC_rate=$GAMMA -d lambda=$LAMBDA -d "File_path_i='X_Sampled_individuals_$NAME.vcf'" -d "File_path_f='X_Fixed_mutations_$NAME.csv'" -d "Fitness_mean_file_name='Wm_Mean_fitness_$NAME.csv'" -d "Fitness_variance_file_name='Wv_Variance_fitness_$NAME.csv'" Cloning_with_GC_constant_sh_vXII_60.slim
