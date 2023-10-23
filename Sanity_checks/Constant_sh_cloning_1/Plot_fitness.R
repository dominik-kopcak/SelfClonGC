library(dplyr)
library(ggplot2)

h_values = c(0.2, 0.5)

Max_time=20000

N_replicates_cloning = 100
N_replicates_cloning_GC = 20
colours <- c("Cloning"="red","Cloning+GC"="lightblue")

Fitness_cloning <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_fitness_evolution_cloning.csv")
Fitness_cloning_GC <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/lambda_100/Data_fitness_evolution_GC.csv")


for (h_coefficient in h_values){

	fitness_cloning_subset_0 <- Fitness_cloning %>%
		filter(Dominance_coefficient == h_coefficient,
			Uniparenting_rate == 1.00)
	
	fitness_cloning_GC_subset_0 <- Fitness_cloning_GC %>%
		 filter(Dominance_coefficient == h_coefficient,
			GC_rate == 10**(-10))


	Plot_mean_ <- ggplot(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_mean_fitness), size=1) +

			         geom_line(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_mean_fitness), colour = "red") +
				 geom_ribbon(data=fitness_cloning_subset_0, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates_cloning, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates_cloning, colour = "Cloning"), alpha = 0.5) +
				 
				 geom_line(data=fitness_cloning_GC_subset_0, aes(x=Time, y=Expected_mean_fitness), colour = "lightblue", key_glyph = draw_key_rect) +
				 geom_ribbon(data=fitness_cloning_GC_subset_0, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates_cloning, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates_cloning, colour = "Cloning+GC"), alpha = 0.5)
	
	
	Plot_variance_ <- ggplot(data=fitness_cloning_GC_subset_0, aes(x=Time, y=Expected_variance_fitness), size=1) +

				     geom_line(data=fitness_cloning_subset_0, aes(x=Time, y=Expected_variance_fitness), colour = "red")+
				     geom_ribbon(data=fitness_cloning_subset_0, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates_cloning_GC), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates_cloning_GC), colour = "Cloning"), alpha = 0.5) +
				     
				     geom_line(data=fitness_cloning_GC_subset_0, aes(x=Time, y=Expected_variance_fitness), colour = "lightblue", key_glyph = draw_key_rect)+
				     geom_ribbon(data=fitness_cloning_GC_subset_0, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates_cloning_GC, ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates_cloning_GC, colour = "Cloning+GC"), alpha = 0.5)
	
	Plot_mean_ <- Plot_mean_ +
		labs(title="Population fitness\nunder  reproduction",
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
	    	   
		    	   
	 Plot_variance_ <- Plot_variance_ +
		labs(title="Population fitness variance\nunder  reproduction",
		     x="Time",
		     y="Fitness variance",
		     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = #, sample size = 50, run for # 000 generations", sep=""))+

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



plot_mean__name <- paste("Figure_mean_fitness", h_coefficient, ".png", sep="")
ggsave(filename = plot_mean__name,
	plot = Plot_mean_,
	dpi = 600)

plot_variance__name <- paste("Figure_variance_fitness", h_coefficient, ".png", sep="")
ggsave(filename = plot_variance__name,
	plot = Plot_variance_,
	dpi = 600)
	
}
