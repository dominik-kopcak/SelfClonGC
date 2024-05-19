library(dplyr)
library(ggplot2)

gamma_values = c(10**seq(-5, -11, -1))

Max_time = 60000

N_replicates = 10


Fitness_GC <- read.csv("Data_fitness_evolution_GC_nonconstant_sh_60000.csv")
Fitness_GC$GC_rate <- as.factor(Fitness_GC$GC_rate)


fitness_GC_subset_0 <- Fitness_GC %>%
	 filter(GC_rate == min(gamma_values))

Plot_mean_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_mean_fitness), size=1)
Plot_variance_GC <- ggplot(data=fitness_GC_subset_0, aes(x=Time, y=Expected_variance_fitness), size=1)

for (rate in gamma_values){
		
	fitness_GC_subset <- Fitness_GC %>%
	filter(GC_rate == rate)
			
	Plot_mean_GC <- Plot_mean_GC +
		geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_mean_fitness, colour = GC_rate))+
		geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates), ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates), fill = GC_rate), alpha = 0.5)
			
	Plot_variance_GC <- Plot_variance_GC +
		geom_line(data=fitness_GC_subset, aes(x=Time, y=Expected_variance_fitness, colour = GC_rate))+
		geom_ribbon(data=fitness_GC_subset, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates), ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates), fill = GC_rate), alpha = 0.5)
	}

Plot_mean_GC <- Plot_mean_GC +
	scale_colour_viridis_d(guide = "none") + 
	scale_fill_viridis_d() +
	labs(title="Population Fitness\nUnder Asexual Reproduction with GC",
 	    x="Time",
 	    y="Mean fitness",
 	    caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 4 000 bp,\nnumber of replicates = 10, sample size = 50, run for 60 000 generations", sep=""))+
	guides(fill = guide_legend(title = "GC rate")) +

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
   
Plot_variance_GC <- Plot_variance_GC +
	scale_colour_viridis_d(guide = "none") + 
	scale_fill_viridis_d() +  
	labs(title="Population Fitness\nUnder Asexual Reproduction with GC",
	 x="Time",
    	 y="Fitness variance",
    	 caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 4 000 bp,\nnumber of replicates = 10, sample size = 50, run for 60 000 generations", sep=""))+
	guides(fill = guide_legend(title = "GC rate")) +

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

plot_mean_GC_name <- paste("Figure_mean_fitness_GC_nonconstant_sh.png", sep="")
ggsave(filename = plot_mean_GC_name,
plot = Plot_mean_GC,
dpi = 600)

plot_variance_GC_name <- paste("Figure_variance_fitness_GC_nonconstant_sh.png", sep="")
ggsave(filename = plot_variance_GC_name,
plot = Plot_variance_GC,
dpi = 600)

