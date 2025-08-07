#!/bin/sh
# Runs GC sims with larger N (10,000)

# Grid Engine options (lines prefixed with #SBATCH)
#SBATCH --job-name=CloneGC_largeN    # Job name
#SBATCH --export=ALL								  # Use activated conda env
#SBATCH --array=121-140              		 	  	  # Array tasks
#SBATCH --mem=8gb	                     			  # Job memory request
#SBATCH --time=48:00:00              				  # Time limit hrs:min:sec
#SBATCH --ntasks=1                  
#SBATCH --cpus-per-task=1           				  # CPU request
#SBATCH --output=/mnt/loki/hartfield/AsexMuts/scripts/output/%x_%a.out
#SBATCH --error=/mnt/loki/hartfield/AsexMuts/scripts/error/%x_%a.err

# Running simulations with parameters stored in 'parameters_vXII_full_60_largeN.txt'
SEL=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_vXII_full_60_largeN.txt | awk '{print $1}')
DOM=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_vXII_full_60_largeN.txt | awk '{print $2}')
GAMMA=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_vXII_full_60_largeN.txt | awk '{print $3}')
LAMBDA=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_vXII_full_60_largeN.txt | awk '{print $4}')
REP=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_vXII_full_60_largeN.txt | awk '{print $5}')

NAME=s${SEL}_h${DOM}_gamma${GAMMA}_lambda${LAMBDA}_replicate${REP}_vXII_full_60_largeN

cd /mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/4_new_pop_size/1_SLiM_2_con_newpop/
slim -d s_coeff=$SEL -d h_coeff=$DOM -d GC_rate=$GAMMA -d lambda=$LAMBDA -d "File_path_i='X_Sampled_individuals_$NAME.vcf'" -d "File_path_f='X_Fixed_mutations_$NAME.csv'" -d "Fitness_mean_file_name='Wm_Mean_fitness_$NAME.csv'" -d "Fitness_variance_file_name='Wv_Variance_fitness_$NAME.csv'" Cloning_with_GC_constant_sh_vXII_60_largeN.slim
mv X_Sampled_individuals_$NAME.vcf X_Fixed_mutations_$NAME.csv Wm_Mean_fitness_$NAME.csv Wv_Variance_fitness_$NAME.csv /mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/4_new_pop_size/3_data_newpop/
