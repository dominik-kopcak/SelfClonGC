#!/bin/sh
# Plotting fitness metrics for new sims with new pop size

# Grid Engine options (lines prefixed with #SBATCH)
#SBATCH --job-name=FitCalc_smallN    # Job name
#SBATCH --export=ALL								  # Use activated conda env
#SBATCH --mem=8gb	                     			  # Job memory request
#SBATCH --time=1:00:00              				  # Time limit hrs:min:sec
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1           				  # CPU request
#SBATCH --output=/mnt/loki/hartfield/AsexMuts/scripts/output/%x_%a.out
#SBATCH --error=/mnt/loki/hartfield/AsexMuts/scripts/error/%x_%a.err

Rscript Calculate_fitness_lambda_4000_smallpop.R
Rscript Plot_fitness_lambda_4000_smallpop.R

Rscript Calculate_mutation_accumulation_4000_smallpop.R
Rscript Plot_mutation_accumulation_lambda_4000_smallpop.R
