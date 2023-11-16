library(dplyr)
library(ggplot2)

Uniparenting_rates <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.95, 0.99, 1.00)
GC_rates <- c(10**seq(-10, -4, 1))

h_values = c(0.2, 0.5)

Max_time=20000

N_replicates = 100

GC_colour_scheme <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
Selfing_colour_scheme <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b')

Fitness_selfing <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_fitness_evolution_selfing.csv")
Fitness_selfing$Uniparenting_rate <- as.factor(Fitness_selfing$Uniparenting_rate)

Fitness_GC <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/lambda_100/Data_fitness_evolution_GC.csv")
Fitness_GC$GC_rate <- as.factor(Fitness_GC$GC_rate)

### h = 0.2

Plot_mean_selfing <- ggplot() +
	geom_line(data=subset(Fitness_selfing, Dominance_coefficient %in% 0.2), aes(x=Time, y=Expected_mean_fitness, fill = Uniparenting_rate))+
	# geom_ribbon(data=subset(Fitness_selfing, Dominance_coefficient %in% 0.2), aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates, fill = Uniparenting_rate)) +
	
	geom_line(data=subset(Fitness_GC, Dominance_coefficient %in% 0.2), aes(x=Time, y=Expected_mean_fitness, colour = GC_rate))
	# geom_ribbon(aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/N_replicates, ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/N_replicates, fill = GC_rate), alpha = 0.5)




Plot_variance_selfing <- ggplot(data=subset(Fitness_selfing, Dominance_coefficient %in% 0.2)) +
	# geom_line(aes(x=Time, y=Expected_variance_fitness, colour = Uniparenting_rate))+
	geom_ribbon(aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates), fill = Uniparenting_rate), alpha = 0.5)
	
Plot_variance_selfing <- ggplot(data=subset(Fitness_GC, Dominance_coefficient %in% 0.2)) +
	geom_line(aes(x=Time, y=Expected_variance_fitness, colour = GC_rate,))
	# geom_ribbon(aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/N_replicates), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/N_replicates), fill = Uniparenting_rate), alpha = 0.5)
			


	
Plot_mean_selfing <- Plot_mean_selfing +
		
		scale_colour_manual(name="GC rate",
				    values = GC_colour_scheme) + 
		
		scale_fill_manual(name="Selfing rate",
				  values = Selfing_colour_scheme) + 
		
		labs(title="Population fitness\nselfing vs cloning with GC",
		     x="Time",
		     y="Mean fitness",
		     caption=paste("s = 0.01, h = 0.2, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = #, sample size = 50, run for # 000 generations", sep=""))+
		     
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
	 
		scale_colour_viridis_d(name="Legend 1") + 
		
		scale_fill_viridis_d(name="Legend 1",
				     option = "plasma") + 
		labs(title="Population fitness variance\nunder selfing reproduction",
		     x="Time",
		     y="Fitness variance",
		     caption=paste("s = 0.01, h = 0.2, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = #, sample size = 50, run for # 000 generations", sep=""))+
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


ggsave(filename = "Figure_mean_fitness_selfing_vs_GC_0.2.png",
	plot = Plot_mean_selfing,
	dpi = 600)

ggsave(filename = "Figure_variance_fitness_selfing_vs_GC_0.2.png",
	plot = Plot_variance_selfing,
	dpi = 600)



### h = 0.5
