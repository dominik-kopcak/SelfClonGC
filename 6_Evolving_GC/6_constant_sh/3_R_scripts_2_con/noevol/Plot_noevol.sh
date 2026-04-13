#!/bin/sh
# Plot 'no evolution' results

# Grid Engine options (lines prefixed with #SBATCH)
#SBATCH --job-name=Plot_GC_noevol    # Job name
#SBATCH --export=ALL								  # Use activated conda env
#SBATCH --mem=8gb	                     			  # Job memory request
#SBATCH --time=1:00:00              				  # Time limit hrs:min:sec
#SBATCH --ntasks=1                 
#SBATCH --cpus-per-task=1           				  # CPU request
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=m.hartfield@ed.ac.uk  # Where to send mail
#SBATCH --output=/mnt/loki/hartfield/AsexMuts/scripts/output/%x_%a.out
#SBATCH --error=/mnt/loki/hartfield/AsexMuts/scripts/error/%x_%a.err

echo -e "Calculating Fitness\n"
Rscript Calculate_fitness_lambda_4000_noevol.R
echo -e "Calculating Mutation Accumulation\n"
Rscript Calculate_mutation_accumulation_4000_noevol.R

echo -e "Plotting Fitness\n"
Rscript Plot_fitness_lambda_4000_noevol.R
echo -e "Plotting Final Fitness\n"
Rscript Plot_final_fitness_lambda_4000_noevol.R
echo -e "Plotting Mutation Accumulation\n"
Rscript Plot_mutation_accumulation_lambda_4000_noevol.R
