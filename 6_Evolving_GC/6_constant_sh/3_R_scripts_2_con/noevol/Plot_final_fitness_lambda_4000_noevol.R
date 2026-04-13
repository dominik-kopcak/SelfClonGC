library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)
library(magrittr)

GC_rates = c(10**seq(-5, -11, -1))

h_values = c(0.2)

N_replicates = 20

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/6_Evolving_GC/6_constant_sh/4_Data_2_con")

Fitness_GC <- read.csv("Data_fitness_evolution_GC_noevol.csv")

for (h_coefficient in h_values){
	
	Fitness_GC_final <- Fitness_GC %>% filter(Dominance_coefficient == h_coefficient, Time == 60000)
	Fitness_GC_final$GC_rate <- Fitness_GC_final$GC_rate*1000 # To account for mean GC length

	Plot_final_fit_GC <- ggplot() +
	geom_point(data= Fitness_GC_final,aes(x=GC_rate, y=Expected_mean_fitness), size=3, shape=15)+
		geom_linerange(data= Fitness_GC_final,aes(x=GC_rate, 
				   ymin=(Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates)),
				   ymax=(Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates))), linewidth=1) +

	labs(x=expression("Mean GC rate per site (" %*% "4 )"),
		     y="Final mean fitness") +
			     
		theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	     
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_text(size=15),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15))+

	scale_x_log10(breaks = c(10**seq(-8, -2, 1)),
                     labels = trans_format("log10", math_format(10^.x)))+
	scale_y_continuous(breaks=c(seq(0, 1, 0.2)),limits=c(0,1)) 

plot_mean_GC_name_final <- paste("Figure_final_mean_fitness_GC_", h_coefficient, "_noevol.png", sep="")
ggsave(filename = plot_mean_GC_name_final,
	plot = 	Plot_final_fit_GC,
	dpi = 600)
}
