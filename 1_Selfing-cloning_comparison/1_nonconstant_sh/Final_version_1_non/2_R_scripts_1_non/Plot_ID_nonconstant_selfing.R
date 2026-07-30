#!/usr/bin/env Rscript

library(tidyverse)

ID_df_nonconstant_selfing <- read.csv("Data_ID_nonconstant_selfing.csv")
ID_df_nonconstant_selfing$Rep <- as.factor(ID_df_nonconstant_selfing$Rep)
ID_df_nonconstant_selfing$Rate <- as.factor(ID_df_nonconstant_selfing$Rate)

replicate_number = length(unique(ID_df_nonconstant_selfing$Rep))

ID_df_nonconstant_selfing_sum <- ID_df_nonconstant_selfing %>%
	group_by(Rate) %>%
	summarise(Expected_g2=mean(g2), sd_g2=sd(g2))

print(replicate_number)


print(str(ID_df_nonconstant_selfing))

g2_plot <- ggplot() + 
	geom_linerange(data = ID_df_nonconstant_selfing, aes(x = Rate, ymin = CI_lower, ymax = CI_upper), size = 2, alpha =0.05)+
	geom_point(data = ID_df_nonconstant_selfing_sum, aes(x=Rate, y=Expected_g2), size = 4) + 
	geom_errorbar(data = ID_df_nonconstant_selfing_sum, aes(x=Rate, ymin=Expected_g2-1.96*sd_g2/sqrt(replicate_number), ymax=Expected_g2+1.96*sd_g2/sqrt(replicate_number))) +
    scale_color_brewer(palette = "RdBu") + 
	labs(x="Selfing rate",y=expression(paste("Identity disequilibrium (",g[2],")"))) + 
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
	
ggsave(filename = "Plot_ID_g2_nonconstant_selfing.png",
	plot = g2_plot,
	dpi = 600,
	width=12,
	height=12)

print(warnings())
