library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)
library(tidyverse)

h_values = c(0.2)

N_replicates = 20

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/6_Evolving_GC/6_constant_sh/4_Data_2_con")

Fitness_GC_in <- read.csv("Data_fitness_evolution_GC_WithEvol.csv")

Fitness_GC <- transform(Fitness_GC_in,
	GC_var = factor(GC_var, levels=unique(Fitness_GC_in$GC_var), labels=c(expression("Standard Deviation = " ~ 1 %*% 10 ^ -8), expression("Standard Deviation = " ~ 1 %*% 10 ^ -7))))
Fitness_GC$GC_rate <- as.factor(Fitness_GC$GC_rate)

for (h_coefficient in h_values){
	
	fitness_GC_subset_0 <- Fitness_GC %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 0)
	
	Plot_mean_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_mean_fitness))
	Plot_variance_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_variance_fitness))
	Plot_evol_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_GC))
	
	for (rate in unique(Fitness_GC$GC_rate)){
	
		fitness_GC_subset <- Fitness_GC %>%
			filter(Dominance_coefficient == h_coefficient,
				GC_rate == rate)
		
		psize <- 1
			
		Plot_mean_GC <- Plot_mean_GC +
			geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_mean_fitness, colour = GC_rate), size = psize) +
			geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=CI_fit_L, ymax=CI_fit_U, fill = GC_rate), alpha = 0.5)
			
		Plot_variance_GC <- Plot_variance_GC +
			geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_variance_fitness, colour = GC_rate), size = psize) +
			geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=CI_var_L, ymax=CI_var_U, fill = GC_rate), alpha = 0.5)
			
		Plot_evol_GC <- Plot_evol_GC +
			geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_GC, colour = GC_rate), size = psize) +
			geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=CI_GC_L, ymax= CI_GC_U, fill = GC_rate), alpha = 0.5)
	
	}
	  	   
	 Plot_mean_GC <- Plot_mean_GC +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d(breaks=unique(Fitness_GC$GC_rate), labels=c(expression(4 %*% 10 ^ -7), expression(4 %*% 10 ^ -4), expression(4 %*% 10 ^ -2))) +
		facet_grid(cols = vars(GC_var), labeller = label_parsed) +
		labs(x="Time",
		     y="Mean fitness")+
		guides(fill = guide_legend(title = "Starting gene conversion\nrate per site")) +
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
    	   	   strip.text = element_text(face = "bold", size = rel(2.5)),
    	   	   panel.spacing = unit(6, "lines"),
    	   	   
	    	   plot.title=element_text(size=40, hjust=0.5, face="bold"),
	    	   plot.caption=element_text(size=20, hjust=0.5),
	    	   axis.title.y=element_text(size=40),
	    	   axis.title.x=element_text(size=40),
	      
	    	   axis.text.x=element_text(size=40),
	    	   axis.text.y=element_text(size=40),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.title = element_text(size=30),
	    	   legend.text = element_text(size=30),
	    	   legend.key.size = unit(1.5, "cm"))
	    	   
	  Plot_variance_GC <- Plot_variance_GC +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d(breaks=unique(Fitness_GC$GC_rate), labels=c(expression(4 %*% 10 ^ -7), expression(4 %*% 10 ^ -4), expression(4 %*% 10 ^ -2))) + 
		facet_grid(cols = vars(GC_var), labeller = label_parsed) +
		labs(x="Time",
		     y="Fitness variance")+
		guides(fill = guide_legend(title = "Starting gene conversion\nrate per site")) +
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
       	   	   strip.text = element_text(face = "bold", size = rel(2.5)),
       	   	   panel.spacing = unit(6, "lines"),
    	   	   
	    	   plot.title=element_text(size=40, hjust=0.5, face="bold"),
	    	   plot.caption=element_text(size=20, hjust=0.5),
	    	   axis.title.y=element_text(size=40),
	    	   axis.title.x=element_text(size=40),
	      
	    	   axis.text.x=element_text(size=40),
	    	   axis.text.y=element_text(size=40),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.title = element_text(size=30),
	    	   legend.text = element_text(size=30),
	    	   legend.key.size = unit(1.5, "cm"))
	  
	  Plot_evol_GC <- Plot_evol_GC +
  	 	scale_y_log10() +
		scale_colour_viridis_d(guide = "none") + 
		scale_fill_viridis_d(breaks=unique(Fitness_GC$GC_rate), guide = "none") +
		facet_grid(cols = vars(GC_var), labeller = label_parsed) +
		labs(x="Time",
		     y="Mitotic gene conversion rate")+
	     
		theme(panel.grid.major=element_blank(),
    	           panel.grid.minor=element_blank(),
    	  	   panel.background=element_blank(),
    	   	   axis.line = element_line(color="black"),
    	   	   
    	   	   strip.text = element_text(face = "bold", size = rel(2.5)),
       	   	   panel.spacing = unit(6, "lines"),    	   	   
    	   	   
	    	   plot.title=element_text(size=40, hjust=0.5, face="bold"),
	    	   plot.caption=element_text(size=20, hjust=0.5),
	    	   axis.title.y=element_text(size=40),
	    	   axis.title.x=element_text(size=40),
	      
	    	   axis.text.x=element_text(size=40),
	    	   axis.text.y=element_text(size=40),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.title = element_text(size=30),
	    	   legend.text = element_text(size=30),
	    	   legend.key.size = unit(1.5, "cm"))

Plot_all <- Plot_mean_GC/Plot_evol_GC

plot_mean_GC_name <- paste("Figure_mean_fitness_GC_", h_coefficient, "_WithEvol.png", sep="")
ggsave(filename = plot_mean_GC_name,
	plot = Plot_mean_GC,
	dpi = 300,
	width = 24,
	height = 12)

plot_variance_GC_name <- paste("Figure_variance_fitness_GC_", h_coefficient, "_WithEvol.png", sep="")
ggsave(filename = plot_variance_GC_name,
	plot = Plot_variance_GC,
	dpi = 300,
	width = 24,
	height = 12)

plot_evolving_GC_name <- paste("Figure_evolving_GC_", h_coefficient, "_WithEvol.png", sep="")
ggsave(filename = plot_evolving_GC_name,
	plot = Plot_evol_GC,
	dpi = 300,
	width = 24,
	height = 12)

plot_all_name <- paste("Figure_all_", h_coefficient, "_WithEvol.png", sep="")
ggsave(filename = plot_all_name,
	plot = Plot_all,
	dpi = 300,
	width = 24,
	height = 24)
}
