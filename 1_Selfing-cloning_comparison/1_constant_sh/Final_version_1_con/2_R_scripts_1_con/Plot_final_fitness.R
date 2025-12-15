library(dplyr)
library(ggplot2)

Uniparenting_rates <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.95, 0.99, 1.00)

h_values = c(0.2)

N_replicates = 100

Fitness_selfing <- read.csv("Data_fitness_evolution_selfing_full_60000.csv")

Fitness_cloning <- read.csv("Data_fitness_evolution_cloning_full_60000.csv")

for (h_coefficient in h_values){

        fitness_selfing_subset <- Fitness_selfing %>%
		 filter(Time == 30000) 		# Note, timestep 30,000 corresponds to 60,000 generations as printout occured every 2nd generation
	
	fitness_cloning_subset <- Fitness_cloning %>%
		 filter(Time == 30000)
	
	Plot_final_fit <- ggplot() +
		geom_point(data=fitness_selfing_subset,aes(x=Uniparenting_rate, y=Expected_mean_fitness, colour="Selfing"), size=3, shape=15)+
		geom_linerange(data=fitness_selfing_subset,aes(x=Uniparenting_rate, 
				   ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)),
				   ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), colour="darkolivegreen2", linewidth=1) +

		geom_point(data=fitness_cloning_subset,aes(x=Uniparenting_rate, y=Expected_mean_fitness, colour="Asexuality"), size=3, shape=15) +
		geom_linerange(data=fitness_cloning_subset,aes(x=Uniparenting_rate, 
				   ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)), 
				   ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), colour="darkolivegreen", linewidth=1) +

		labs(x="Rate of uniparental reproduction",
		     y="Final mean fitness",
		     colour="Reproduction: ") +
			     
		theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	     
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_text(size=15),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position=c(0.5, 0.4),
	      legend.box.background = element_rect(color="black", linewidth=2))+

	guides(colour = guide_legend(override.aes = list(size = 8)))+
	scale_x_continuous(breaks=c(seq(0, 1, 0.2))) +
	scale_y_continuous(breaks=c(seq(0, 1, 0.2)),limits=c(0,1)) +
	scale_colour_manual(values = c("Selfing"="darkolivegreen2","Asexuality"="darkolivegreen"),breaks=c("Selfing","Asexuality"))
	    	    
plot_mean_fitness_name <- paste("Figure_mean_final_fitness_", h_coefficient, "_full_60000.png", sep="")
ggsave(filename = plot_mean_fitness_name,
	plot = Plot_final_fit,
	dpi = 600)
	
}
