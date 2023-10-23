library(dplyr)
library(ggplot2)

h_values = c(0.2, 0.5)

Max_time=20000

N_replicates = 100

colours <- c("Selfing"="darkolivegreen2","Cloning"="darkolivegreen")

Fitness_selfing <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_fitness_evolution_selfing.csv")
Fitness_selfing$Uniparenting_rate <- as.factor(Fitness_selfing$Uniparenting_rate)

Fitness_cloning <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_fitness_evolution_cloning.csv")
Fitness_cloning$Uniparenting_rate <- as.factor(Fitness_cloning$Uniparenting_rate)

for (h_coefficient in h_values){

        fitness_selfing_subset_0 <- Fitness_selfing %>%
		 filter(Dominance_coefficient == h_coefficient,
			Uniparenting_rate == 0)
	
	fitness_cloning_subset_0 <- Fitness_cloning %>%
		 filter(Dominance_coefficient == h_coefficient,
			Uniparenting_rate == 0)
			
	fitness_selfing_cloning_subset_0 <- list(fitness_selfing_subset_0, fitness_cloning_subset_0)
	
	
	Plot_mean_outcrossing <- ggplot(data=fitness_selfing_subset_0, aes(x=Time, y=Expected_mean_fitness), size=1) +

			         geom_line(data=fitness_selfing_subset_0, aes(x=Time, y=Expected_mean_fitness), colour = "darkolivegreen2", key_glyph = draw_key_rect) +
				 geom_ribbon(data=fitness_selfing_subset_0, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates, colour = "Selfing"), alpha = 0.5) +
				 
				 geom_line(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_mean_fitness), colour = "darkolivegreen") +
				 geom_ribbon(data=fitness_cloning_subset_0, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates, colour = "Cloning"), alpha = 0.5, key_glyph = draw_key_rect)
	
	
	Plot_variance_outcrossing <- ggplot(data=fitness_selfing_subset_0, aes(x=Time, y=Expected_variance_fitness), size=1) +

				     geom_line(data=fitness_selfing_subset_0, aes(x=Time, y=Expected_variance_fitness), colour = "darkolivegreen2")+
				     geom_ribbon(data=fitness_selfing_subset_0, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates), colour = "Selfing"), alpha = 0.5) +
				     
				     geom_line(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_variance_fitness), colour = "darkolivegreen")+
				     geom_ribbon(data=fitness_cloning_subset_0, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates, ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates, colour = "Cloning"), alpha = 0.5, key_glyph = draw_key_rect)
	
	Plot_mean_outcrossing <- Plot_mean_outcrossing +
		labs(title="Population fitness\nunder outcrossing reproduction",
		     x="Time",
		     y="Mean fitness",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 100, sample size = 50, run for 5 000 generations", sep=""))+
	
		scale_colour_manual(values = colours)+
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
    	   	   plot.title=element_text(size=20, hjust=0.5, face="bold"),
	  	   plot.caption=element_text(size=10, hjust=0.5),
	   	   plot.subtitle=element_text(size=15, hjust=1, face="bold"), 
	   	   
	   	   axis.title.y=element_text(size=15),
	    	   axis.title.x=element_text(size=15),
	      
	    	   axis.text.x=element_text(size=15),
	    	   axis.text.y=element_text(size=15),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.key.size = unit(1, "cm"))
	    	   
		    	   
	 Plot_variance_outcrossing <- Plot_variance_outcrossing +
		labs(title="Population fitness variance\nunder outcrossing reproduction",
		     x="Time",
		     y="Fitness variance",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 100, sample size = 50, run for 5 000 generations", sep=""))+

		scale_colour_manual(values = colours)+
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
    	   	   plot.title=element_text(size=20, hjust=0.5, face="bold"),
	  	   plot.caption=element_text(size=10, hjust=0.5),
	   	   plot.subtitle=element_text(size=15, hjust=1, face="bold"), 
	   	   
	   	   axis.title.y=element_text(size=15),
	    	   axis.title.x=element_text(size=15),
	      
	    	   axis.text.x=element_text(size=15),
	    	   axis.text.y=element_text(size=15),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.key.size = unit(1, "cm"))



plot_mean_outcrossing_name <- paste("Figure_mean_fitness_outcrossing_", h_coefficient, ".png", sep="")
ggsave(filename = plot_mean_outcrossing_name,
	plot = Plot_mean_outcrossing,
	dpi = 600)

plot_variance_outcrossing_name <- paste("Figure_variance_fitness_outcrossing_", h_coefficient, ".png", sep="")
ggsave(filename = plot_variance_outcrossing_name,
	plot = Plot_variance_outcrossing,
	dpi = 600)
	
}
