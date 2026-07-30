#!/usr/bin/env Rscript

library(tidyverse)
library(scales)

ID_df_GC <- read.csv("Data_ID_GC.csv")
ID_df_GC$Rep <- as.factor(ID_df_GC$Rep)

replicate_number = length(unique(ID_df_GC$Rep))

ID_df_GC_sum <- ID_df_GC %>%
	group_by(Rate) %>%
	summarise(Expected_g2=mean(g2), sd_g2=sd(g2))

print(replicate_number)


print(str(ID_df_GC))

g2_plot <- ggplot() + 
	geom_linerange(data = ID_df_GC, aes(x=Rate*1000, ymin=CI_lower, ymax = CI_upper), size = 2, alpha = 0.01) +
	geom_point(data = ID_df_GC_sum, aes(x=Rate*1000, y=Expected_g2), size = 4) + 
	geom_errorbar(data = ID_df_GC_sum, aes(x=Rate*1000, ymin=Expected_g2-1.96*sd_g2/sqrt(replicate_number), ymax=Expected_g2+1.96*sd_g2/sqrt(replicate_number))) +
    scale_color_brewer(palette = "RdBu") + 
	labs(x=expression("Mean GC rate per site (" %*% "4 )"), y=expression(paste("Identity disequilibrium (",g[2],")"))) + 
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
	      
	      legend.position="none")+
	scale_x_log10(breaks = c(10**seq(-8, -2, 1)),
                     labels = trans_format("log10", math_format(10^.x)))
	
ggsave(filename = "Plot_ID_g2_GC.png",
	plot = g2_plot,
	dpi = 300,
	width=12,
	height=12)

print(warnings())
