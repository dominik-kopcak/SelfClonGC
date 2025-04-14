#!/usr/bin/env Rscript

library(tidyverse)

ID_df_cloning <- read.csv("Data_ID_cloning.csv")
ID_df_cloning$Rep <- as.factor(ID_df_cloning$Rep)
ID_df_cloning$Rate <- as.factor(ID_df_cloning$Rate)

replicate_number = length(unique(ID_df_cloning$Rep))

ID_df_cloning_sum <- ID_df_cloning %>%
	group_by(Rate) %>%
	summarise(Expected_g2=mean(g2), sd_g2=sd(g2))


print(replicate_number)


print(str(ID_df_cloning))

g2_plot <- ggplot() + 
	geom_linerange(data = ID_df_cloning, aes(x=Rate, ymin=CI_lower, ymax=CI_upper), size = 2, alpha = 0.01) +
	geom_point(data = ID_df_cloning_sum, aes(x=Rate, y=Expected_g2), size = 4) + 
	geom_errorbar(data = ID_df_cloning_sum, aes(x=Rate, ymin=Expected_g2-1.96*sd_g2/sqrt(replicate_number), ymax=Expected_g2+1.96*sd_g2/sqrt(replicate_number))) +
    scale_color_brewer(palette = "RdBu") + 
	labs(x="Cloning rate",y=expression(paste("Identity disequilibrium (",g[2],")")), 
		title="ID for asexual populations",
		caption=paste("s = 0.01, h = 0.2, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8, \nnumber of replicates = ", replicate_number, ", sample size = 50, run for 60 000 generations\n95% CI of g2 replicate mean and 95% of CI of each g2 estimates are shown\nwith alpha scaled to be 1 for all replicates", sep="")) + 
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
	      
	      legend.position="none")
	     

ggsave(filename = "Plot_ID_g2_cloning.png",
	plot = g2_plot,
	dpi = 600,
	width=12,
	height=12)

print(warnings())
