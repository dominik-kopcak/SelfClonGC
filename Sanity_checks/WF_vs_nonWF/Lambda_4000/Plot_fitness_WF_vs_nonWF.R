library(dplyr)
library(ggplot2)

h_coefficient = 0.2

Max_time=60000

N_replicates = 100
colours <- c("Complete cloning"="red","Complete cloning + GC  rate 1E-09"="#45D8FB", "Complete cloning + GC  rate 1E-10"="#04C6F5", "Complete cloning + GC  rate 1E-11"="#038AEF")

Fitness_cloning <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_60/Data_fitness_evolution_cloning_full_60000.csv")
Fitness_cloning_GC <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/Model_december/Full_60000/Data_fitness_evolution_GC_vXII_full_60_9-11.csv")


fitness_cloning <- Fitness_cloning %>%
	filter(Dominance_coefficient == h_coefficient,
		Uniparenting_rate == 1.00)

fitness_cloning_GC_09 <- Fitness_cloning_GC %>%
	 filter(Dominance_coefficient == h_coefficient,
		GC_rate == 10**(-9))
		
fitness_cloning_GC_10 <- Fitness_cloning_GC %>%
	 filter(Dominance_coefficient == h_coefficient,
		GC_rate == 10**(-10))

fitness_cloning_GC_11 <- Fitness_cloning_GC %>%
	 filter(Dominance_coefficient == h_coefficient,
		GC_rate == 10**(-11))
	
Plot_mean_ <- ggplot() +
			 geom_line(data=fitness_cloning_GC_09, aes(x=Time, y=Expected_mean_fitness, colour = "Complete cloning + GC  rate 1E-09")) +
			 geom_ribbon(data=fitness_cloning_GC_09, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), color = "#45D8FB", alpha = 0.5) +
			 
			 geom_line(data=fitness_cloning_GC_10, aes(x=Time, y=Expected_mean_fitness, colour = "Complete cloning + GC  rate 1E-10")) +
			 geom_ribbon(data=fitness_cloning_GC_10, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), fill = "#04C6F5", alpha = 0.5) +
			 
			 geom_line(data=fitness_cloning_GC_11, aes(x=Time, y=Expected_mean_fitness, colour = "Complete cloning + GC  rate 1E-11")) +
			 geom_ribbon(data=fitness_cloning_GC_11, aes(x=Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), fill = "#038AEF", alpha = 0.5) +
		 
			 geom_line(data=fitness_cloning, aes(x=2*Time, y=Expected_mean_fitness, colour = "Complete cloning")) +
			 geom_ribbon(data=fitness_cloning, aes(x=2*Time, ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)), ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), fill = "red", alpha = 0.5)
	
	
	
Plot_variance_ <- ggplot() +
			     geom_line(data=fitness_cloning_GC_09, aes(x=Time, y=Expected_variance_fitness*1000, colour = "Complete cloning + GC  rate 1E-09"))+
			     geom_ribbon(data=fitness_cloning_GC_09, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, fill = "Complete cloning + GC  rate 1E-09"), alpha = 0.5) +
			     
			     geom_line(data=fitness_cloning_GC_10, aes(x=Time, y=Expected_variance_fitness*1000, colour = "Complete cloning + GC  rate 1E-10"))+
			     geom_ribbon(data=fitness_cloning_GC_10, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, fill = "Complete cloning + GC  rate 1E-10"), alpha = 0.5) +
			     
			     geom_line(data=fitness_cloning_GC_11, aes(x=Time, y=Expected_variance_fitness*1000, colour = "Complete cloning + GC  rate 1E-11"))+
			     geom_ribbon(data=fitness_cloning_GC_11, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, fill = "Complete cloning + GC  rate 1E-11"), alpha = 0.5) +
					     
			     geom_line(data=fitness_cloning, aes(x=2*Time, y=Expected_variance_fitness*1000, colour = "Complete cloning"))+
			     geom_ribbon(data=fitness_cloning, aes(x=2*Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates))*1000, fill = "Complete cloning"), alpha = 0.5)


   	
Plot_mean_ <- Plot_mean_ +
	labs(title="Population Fitness\nUndner Asexuality (WF) and GC (non-WF)",
	     x="Time",
	     y="Mean fitness",
	     caption=paste("for both WF and non-WF: s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, number of replicates = 100,\nsample size = 50, run for 60 000 generations\nfor WF only: recombination rate = 4E-8\nfor non-WF only: mean GC tract length = 4 000 bp", sep=""),
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
	    	   
	    	   legend.position=c(0.7, 0.5),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.key.size = unit(1, "cm"))
	    	   
		    	   
 Plot_variance_ <- Plot_variance_ +
	labs(title="Population Fitness Variance\nUndner Asexuality (WF) and GC (non-WF)",
	     x="Time",
	     y="Fitness variance (x 1E-3)",
	     caption=paste("for both WF and non-WF: s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, number of replicates = 100,\nsample size = 50, run for 60 000 generations\nfor WF only: recombination rate = 4E-8\nfor non-WF only: mean GC tract length = 4 000 bp", sep=""),
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
	    	   
	    	   legend.position=c(0.7, 0.5),
	    	   
	    	   legend.key = element_blank(),
	    	   legend.key.size = unit(1, "cm"))



plot_mean__name <- paste("Figure_mean_fitness_WF_cloning_vs_nonWF_GC_", h_coefficient, "_vXII_40000_60.png",sep="")
ggsave(filename = plot_mean__name,
	plot = Plot_mean_,
	dpi = 600)

plot_variance__name <- paste("Figure_variance_fitness_WF_cloning_vs_nonWF_GC_", h_coefficient, "_vXII_40000_60.png",sep="")
ggsave(filename = plot_variance__name,
	plot = Plot_variance_,
	dpi = 600)
