#!/bin/sh

# Runs sims with evolving GC
# But with evolution off for now, as a test

# Grid Engine options (lines prefixed with #SBATCH)
#SBATCH --job-name=Evol_GC_Test    # Job name
#SBATCH --export=ALL								  # Use activated conda env
#SBATCH --array=1-140%64      	        		 	  	  # Array tasks
#SBATCH --mem=8gb	                     			  # Job memory request
#SBATCH --time=48:00:00              				  # Time limit hrs:min:sec
#SBATCH --ntasks=1                  
#SBATCH --cpus-per-task=1           				  # CPU request
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=m.hartfield@ed.ac.uk  # Where to send mail
#SBATCH --output=/mnt/loki/hartfield/AsexMuts/scripts/output/%x_%a.out
#SBATCH --error=/mnt/loki/hartfield/AsexMuts/scripts/error/%x_%a.err

# Running simulations with parameters stored in 'parameters_Evolving_GC_nomut.txt'
SEL=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_Evolving_GC_nomut.txt | awk '{print $1}')
DOM=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_Evolving_GC_nomut.txt | awk '{print $2}')
GAMMA=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_Evolving_GC_nomut.txt | awk '{print $3}')
LAMBDA=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_Evolving_GC_nomut.txt | awk '{print $4}')
REP=$(sed -n ${SLURM_ARRAY_TASK_ID}p parameters_Evolving_GC_nomut.txt | awk '{print $5}')

NAME=s${SEL}_h${DOM}_gamma${GAMMA}_lambda${LAMBDA}_replicate${REP}_noevol

cd /mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/6_Evolving_GC/6_constant_sh/1_SLiM_2_con/
slim -d s_coeff=$SEL -d h_coeff=$DOM -d init_GC_rate=$GAMMA -d lambda=$LAMBDA -d GC_rate_SD=0 -d "File_path_i='X_Sampled_individuals_$NAME.vcf'" -d "File_path_f='X_Fixed_mutations_$NAME.csv'" -d "Fitness_mean_file_name='Wm_Mean_fitness_$NAME.csv'" -d "Fitness_variance_file_name='Wv_Variance_fitness_$NAME.csv'" -d "GC_file_name='GC_mean_values_$NAME.csv'" AsexGC_w_modifier.slim
mv X_Sampled_individuals_$NAME.vcf X_Fixed_mutations_$NAME.csv Wm_Mean_fitness_$NAME.csv Wv_Variance_fitness_$NAME.csv GC_mean_values_$NAME.csv ../2_outputs/
