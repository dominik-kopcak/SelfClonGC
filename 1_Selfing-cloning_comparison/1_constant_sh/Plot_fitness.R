library(dplyr)
library(ggplot2)

Uniparenting_rates <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.95, 0.99, 1.00)

h_values = c(0.2, 0.5)

Max_time=20000

N_replicates = 100

Fitness_selfing <- read.csv("Data_fitness_evolution_selfing.csv")
Fitness_selfing$Uniparenting_rate <- as.factor(Fitness_selfing$Uniparenting_rate)

Fitness_cloning <- read.csv("Data_fitness_evolution_cloning.csv")
Fitness_cloning$Uniparenting_rate <- as.factor(Fitness_cloning$Uniparenting_rate)

for (h_coefficient in h_values){

        fitness_selfing_subset_0 <- Fitness_selfing %>%
		 filter(Dominance_coefficient == h_coefficient,
			Uniparenting_rate == 0)
	
	fitness_cloning_subset_0 <- Fitness_cloning %>%
		 filter(Dominance_coefficient == h_coefficient,
			Uniparenting_rate == 0)
	
	Plot_mean_selfing <- ggplot(data=fitness_selfing_subset_0, aes(x=Time, y=Expected_mean_fitness), size=1)
	Plot_mean_cloning <- ggplot(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_mean_fitness), size=1)
	
	Plot_variance_selfing <- ggplot(data=fitness_selfing_subset_0, aes(x=Time, y=Expected_variance_fitness), size=1)
	Plot_variance_cloning <- ggplot(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_variance_fitness), size=1)
	
	for (rate in Uniparenting_rates){
	
		fitness_selfing_subset <- Fitness_selfing %>%
			filter(Dominance_coefficient == h_coefficient,
				Uniparenting_rate == rate)
	
		fitness_cloning_subset <- Fitness_cloning %>%
			filter(Dominance_coefficient == h_coefficient,
				Uniparenting_rate == rate)
	
		Plot_mean_selfing <- Plot_mean_selfing +
			geom_line(data=fitness_selfing_subset, aes(x=Time, y=Expected_mean_fitness, colour = Uniparenting_rate))+
			geom_ribbon(data=fitness_selfing_subset, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates, fill = Uniparenting_rate), alpha = 0.5)
			
		Plot_mean_cloning <- Plot_mean_cloning +
			geom_line(data=fitness_cloning_subset, aes(x=Time, y=Expected_mean_fitness, colour = Uniparenting_rate))+
			geom_ribbon(data=fitness_cloning_subset, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates, fill = Uniparenting_rate), alpha = 0.5)
				
		Plot_variance_selfing <- Plot_variance_selfing +
			geom_line(data=fitness_selfing_subset, aes(x=Time, y=Expected_variance_fitness, colour = Uniparenting_rate))+
			geom_ribbon(data=fitness_selfing_subset, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates), fill = Uniparenting_rate), alpha = 0.5)
			
		Plot_variance_cloning <- Plot_variance_cloning +
			geom_line(data=fitness_cloning_subset, aes(x=Time, y=Expected_variance_fitness, colour = Uniparenting_rate))+
			geom_ribbon(data=fitness_cloning_subset, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates, ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates, fill = Uniparenting_rate), alpha = 0.5)
	
	}
	
	Plot_mean_selfing <- Plot_mean_selfing +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d() + 
		labs(title="Population fitness\nunder selfing reproduction",
		     x="Time",
		     y="Mean fitness",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 5, sample size = 50, run for 5 000 generations", sep=""))+
		guides(fill = guide_legend(title = "Selfing rate")) +
	     
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
	    	   
	 Plot_mean_cloning <- Plot_mean_cloning +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d() +
		labs(title="Population fitness\nunder cloning reproduction",
		     x="Time",
		     y="Mean fitness",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 5, sample size = 50, run for 5 000 generations", sep=""))+
		guides(fill = guide_legend(title = "Cloning rate")) +
	     
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
	    	   
	 Plot_variance_selfing <- Plot_variance_selfing +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d() + 
		labs(title="Population fitness variance\nunder selfing reproduction",
		     x="Time",
		     y="Fitness variance",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 5, sample size = 50, run for 5 000 generations", sep=""))+
		guides(fill = guide_legend(title = "Selfing rate")) +
	     
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
	   
	  Plot_variance_cloning <- Plot_variance_cloning +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d() +  
		labs(title="Population fitness variance\nunder cloning reproduction",
		     x="Time",
		     y="Fitness variance",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 5, sample size = 50, run for 5 000 generations", sep=""))+
		guides(fill = guide_legend(title = "Cloning rate")) +
	     
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



plot_mean_selfing_name <- paste("Figure_mean_fitness_selfing_", h_coefficient, ".png", sep="")
ggsave(filename = plot_mean_selfing_name,
	plot = Plot_mean_selfing,
	dpi = 600)

plot_mean_cloning_name <- paste("Figure_mean_fitness_cloning_", h_coefficient, ".png", sep="")
ggsave(filename = plot_mean_cloning_name,
	plot = Plot_mean_cloning,
	dpi = 600)

plot_variance_selfing_name <- paste("Figure_variance_fitness_selfing_", h_coefficient, ".png", sep="")
ggsave(filename = plot_variance_selfing_name,
	plot = Plot_variance_selfing,
	dpi = 600)

plot_variance_cloning_name <- paste("Figure_variance_fitness_cloning_", h_coefficient, ".png", sep="")
ggsave(filename = plot_variance_cloning_name,
	plot = Plot_variance_cloning,
	dpi = 600)
}
