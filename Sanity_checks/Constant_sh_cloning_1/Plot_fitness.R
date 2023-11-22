library(dplyr)
library(ggplot2)

h_values = c(0.2, 0.5)

Max_time=20000

N_replicates = 100
colours <- c("Cloning"="red","Cloning+GC_1E-10"="#00BFFF", "Cloning+GC_1E-11"="#00B2EE", "Cloning+GC_1E-12"="#009ACD", "Cloning+GC_1E-13"="#00688B")

Fitness_cloning <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_fitness_evolution_cloning.csv")
Fitness_cloning_GC <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/lambda_100/Data_fitness_evolution_GC.csv")
Fitness_cloning_GC_extra_small <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/lambda_100/Data_fitness_evolution_GC_extra_small.csv")

for (h_coefficient in h_values){

	fitness_cloning <- Fitness_cloning %>%
		filter(Dominance_coefficient == h_coefficient,
			Uniparenting_rate == 1.00)
	
	fitness_cloning_GC <- Fitness_cloning_GC %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 10**(-10))

	fitness_cloning_GC_extra_small_11 <- Fitness_cloning_GC_extra_small %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 10**(-11))
	fitness_cloning_GC_extra_small_12 <- Fitness_cloning_GC_extra_small %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 10**(-12))
	fitness_cloning_GC_extra_small_13 <- Fitness_cloning_GC_extra_small %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 10**(-13))
			
	Plot_mean_ <- ggplot() +
				 geom_line(data=fitness_cloning_GC, aes(x=Time, y=Expected_mean_fitness, colour = "Cloning+GC_1E-10")) +
				 geom_ribbon(data=fitness_cloning_GC, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates), fill = "Cloning+GC_1E-10"), alpha = 0.5) +
	
				 geom_line(data=fitness_cloning_GC_extra_small_11, aes(x=Time, y=Expected_mean_fitness, colour = "Cloning+GC_1E-11")) +
				 geom_ribbon(data=fitness_cloning_GC_extra_small_11, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates), fill = "Cloning+GC_1E-11"), alpha = 0.5) +
	
				 geom_line(data=fitness_cloning_GC_extra_small_12, aes(x=Time, y=Expected_mean_fitness, colour = "Cloning+GC_1E-12")) +
				 geom_ribbon(data=fitness_cloning_GC_extra_small_12, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates), fill = "Cloning+GC_1E-12"), alpha = 0.5) +
				 
				 geom_line(data=fitness_cloning_GC_extra_small_13, aes(x=Time, y=Expected_mean_fitness, colour = "Cloning+GC_1E-13")) +
				 geom_ribbon(data=fitness_cloning_GC_extra_small_13, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates), fill = "Cloning+GC_1E-13"), alpha = 0.5) +
				 
				 geom_line(data=fitness_cloning, aes(x=Time, y=Expected_mean_fitness, colour = "Cloning")) +
				 geom_ribbon(data=fitness_cloning, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates), fill = "Cloning"), alpha = 0.5)
	
	Plot_variance_ <- ggplot() +
				     geom_line(data=fitness_cloning_GC, aes(x=Time, y=Expected_variance_fitness, colour = "Cloning+GC_1E-10"))+
				     geom_ribbon(data=fitness_cloning_GC, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates), fill = "Cloning+GC_1E-10"), alpha = 0.5) +
				     
				     geom_line(data=fitness_cloning_GC_extra_small_11, aes(x=Time, y=Expected_variance_fitness, colour = "Cloning+GC_1E-11"))+
				     geom_ribbon(data=fitness_cloning_GC_extra_small_11, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates, ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates, fill = "Cloning+GC_1E-11"), alpha = 0.5) +
				     
				     geom_line(data=fitness_cloning_GC_extra_small_12, aes(x=Time, y=Expected_variance_fitness, colour = "Cloning+GC_1E-12"))+
				     geom_ribbon(data=fitness_cloning_GC_extra_small_12, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates, ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates, fill = "Cloning+GC_1E-12"), alpha = 0.5) +
							     
				     geom_line(data=fitness_cloning_GC_extra_small_13, aes(x=Time, y=Expected_variance_fitness, colour = "Cloning+GC_1E-13"))+
				     geom_ribbon(data=fitness_cloning_GC_extra_small_13, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates, ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates, fill = "Cloning+GC_1E-13"), alpha = 0.5) +
						     
				     geom_line(data=fitness_cloning, aes(x=Time, y=Expected_variance_fitness, colour = "Cloning"))+
				     geom_ribbon(data=fitness_cloning, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates), fill = "Cloning"), alpha = 0.5)

   	
	Plot_mean_ <- Plot_mean_ +
		labs(title="Comparison of WF and nonWF models",
		     x="Time",
		     y="Mean fitness",
		     caption=paste("IFor both simulation sets: s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 100, sample size = 50, run for 20 000 generations", sep=""),
		     colour="Reproduction: ")+
	
		scale_colour_manual(values = colours) +
		scale_fill_manual(values = colours, guide = "none") + 
		
		guides(colour = guide_legend(override.aes = list(size = 10))) +
	     
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
	    	   
	    	   legend.position=c(0.8, 0.5),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.key.size = unit(1, "cm"))
	    	   
		    	   
	 Plot_variance_ <- Plot_variance_ +
		labs(title="Comparison of WF and nonWF models",
		     x="Time",
		     y="Fitness variance",
		     caption=paste("For both simulation sets: s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 100, sample size = 50, run for 20 000 generations", sep=""),
		     colour="Reproduction: ")+

		scale_colour_manual(values = colours)+
		scale_fill_manual(values = colours, guide = "none") + 
		
		guides(colour = guide_legend(override.aes = list(size = 10)))+
	     
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
	    	   
	    	   legend.position=c(0.8, 0.5),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.key.size = unit(1, "cm"))



plot_mean__name <- paste("Figure_mean_fitness", h_coefficient, ".png", sep="")
ggsave(filename = plot_mean__name,
	plot = Plot_mean_,
	dpi = 600)

plot_variance__name <- paste("Figure_variance_fitness", h_coefficient, ".png", sep="")
ggsave(filename = plot_variance__name,
	plot = Plot_variance_,
	dpi = 600)
	
}
