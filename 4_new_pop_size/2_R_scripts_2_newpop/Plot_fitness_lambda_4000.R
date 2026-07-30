# Modified to PLOT fitness values from larger-pop sims
library(dplyr)
library(ggplot2)

GC_rates = c(10**seq(-5, -11, -1))

h_values = c(0.2)

N_replicates = 20

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/4_new_pop_size/4_plots/")

Fitness_GC <- read.csv("Data_fitness_evolution_GC_vXII_full_60_5-11_newpop.csv")
Fitness_GC$GC_rate <- as.factor(Fitness_GC$GC_rate)

print(GC_rates)
head(Fitness_GC$GC_rate)

for (h_coefficient in h_values){
	
	fitness_GC_subset_0 <- Fitness_GC %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 0)
	
	Plot_mean_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_mean_fitness), size=1)
	Plot_variance_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_variance_fitness), size=1)
	
	for (rate in GC_rates){
	
	
		fitness_GC_subset <- Fitness_GC %>%
			filter(Dominance_coefficient == h_coefficient,
				GC_rate == rate)
			
		Plot_mean_GC <- Plot_mean_GC +
			geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_mean_fitness, colour = GC_rate))+
			geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates), ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates), fill = GC_rate), alpha = 0.5)
			
		Plot_variance_GC <- Plot_variance_GC +
			geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_variance_fitness, colour = GC_rate))+
			geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates), ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates), fill = GC_rate), alpha = 0.5)
	
	}
	  	   
	 Plot_mean_GC <- Plot_mean_GC +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d(labels=c(expression(4 %*% 10 ^ -2), expression(4 %*% 10 ^ -3), expression(4 %*% 10 ^ -4), expression(4 %*% 10 ^ -5), 
				      expression(4 %*% 10 ^ -6), expression(4 %*% 10 ^ -7), expression(4 %*% 10 ^ -8))) +
		labs(x="Time",
		     y="Mean fitness")+
		guides(fill = guide_legend(title = "Mean GC rate\nper site")) +
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
	    	   plot.title=element_text(size=40, hjust=0.5, face="bold"),
	    	   plot.caption=element_text(size=20, hjust=0.5),
	    	   axis.title.y=element_text(size=35),
	    	   axis.title.x=element_text(size=35),
	      
	    	   axis.text.x=element_text(size=35),
	    	   axis.text.y=element_text(size=35),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.title = element_text(size=20),
	    	   legend.text = element_text(size=20),
	    	   legend.key.size = unit(1.5, "cm"))
	    	   
	  Plot_variance_GC <- Plot_variance_GC +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d(labels=c(expression(4 %*% 10 ^ -2), expression(4 %*% 10 ^ -3), expression(4 %*% 10 ^ -4), expression(4 %*% 10 ^ -5), 
				      expression(4 %*% 10 ^ -6), expression(4 %*% 10 ^ -7), expression(4 %*% 10 ^ -8))) + 
		labs(x="Time",
		     y="Fitness variance")+
		guides(fill = guide_legend(title = "Mean GC rate\nper site")) +
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
	    	   plot.title=element_text(size=40, hjust=0.5, face="bold"),
	    	   plot.caption=element_text(size=20, hjust=0.5),
	    	   axis.title.y=element_text(size=35),
	    	   axis.title.x=element_text(size=35),
	      
	    	   axis.text.x=element_text(size=35),
	    	   axis.text.y=element_text(size=35),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.title = element_text(size=20),
	    	   legend.text = element_text(size=20),
	    	   legend.key.size = unit(1.5, "cm"))


plot_mean_GC_name <- paste("Figure_mean_fitness_GC_", h_coefficient, "_vXII_full_60_newpop.png", sep="")
ggsave(filename = plot_mean_GC_name,
	plot = Plot_mean_GC,
	dpi = 300,
	width = 14,
	height = 12)

plot_variance_GC_name <- paste("Figure_variance_fitness_GC_", h_coefficient, "_vXII_full_60_newpop.png", sep="")
ggsave(filename = plot_variance_GC_name,
	plot = Plot_variance_GC,
	dpi = 300,
	width = 14,
	height = 12)
}
