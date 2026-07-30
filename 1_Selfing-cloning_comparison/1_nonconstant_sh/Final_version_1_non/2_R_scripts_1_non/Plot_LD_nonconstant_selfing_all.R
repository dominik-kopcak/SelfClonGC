#!/usr/bin/env Rscript
library(tidyverse)
library(scales)
library(patchwork)

max_ee <- 100
maxd <- 12.5 	

LD_data_0 <- read.csv("Data_nonconstant_selfing0_.csv")
LD_data_02 <- read.csv("Data_nonconstant_selfing0.2_.csv")
LD_data_04 <- read.csv("Data_nonconstant_selfing0.4_.csv")
LD_data_06 <- read.csv("Data_nonconstant_selfing0.6_.csv")
LD_data_08 <- read.csv("Data_nonconstant_selfing0.8_.csv")
LD_data_095 <- read.csv("Data_nonconstant_selfing0.95_.csv")
LD_data_099 <- read.csv("Data_nonconstant_selfing0.99_.csv")
LD_data_1 <- read.csv("Data_nonconstant_selfing1_.csv")

print("---")

# Plotting LD decay (r^2 and Dprime)

# 0
LD_data_0$Rep <- as.factor(LD_data_0$Rep)
r2_plot_0 <- ggplot(LD_data_0, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
		title="Selfing rate = 0",
		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_0$Rep)) , sep = "")) + 
	xlim(0,maxd) + 
	ylim(0,1)
	
# 02
LD_data_02$Rep <- as.factor(LD_data_02$Rep)
r2_plot_02 <- ggplot(LD_data_02, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_02$Rep)) , sep = ""),
		title="Selfing rate = 0.2") + 
	xlim(0,maxd) + 
	ylim(0,1) 

# 04	
LD_data_04$Rep <- as.factor(LD_data_04$Rep)	
r2_plot_04 <- ggplot(LD_data_04, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_04$Rep)) , sep = ""),
		title="Selfing rate = 0.4") + 
	xlim(0,maxd) + 
	ylim(0,1) 
                     
# 06	
LD_data_06$Rep <- as.factor(LD_data_06$Rep)	
r2_plot_06 <- ggplot(LD_data_06, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_06$Rep)) , sep = ""),
		title="Selfing rate = 0.6") + 
	xlim(0,maxd) + 
	ylim(0,1) 

# 08	
LD_data_08$Rep <- as.factor(LD_data_08$Rep)	
r2_plot_08 <- ggplot(LD_data_08, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_08$Rep)) , sep = ""),
		title="Selfing rate = 0.8") + 
	xlim(0,maxd) + 
	ylim(0,1) 
                     
                    
# 095	
LD_data_095$Rep <- as.factor(LD_data_095$Rep)	
r2_plot_095 <- ggplot(LD_data_095, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_095$Rep)) , sep = ""),
		title="Selfing rate = 0.95") + 
	xlim(0,maxd) + 
	ylim(0,1) 
	
# 099	
LD_data_099$Rep <- as.factor(LD_data_099$Rep)	
r2_plot_099 <- ggplot(LD_data_099, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_099$Rep)) , sep = ""),
		title="Selfing rate = 0.99") + 
	xlim(0,maxd) + 
	ylim(0,1) 
	
# 1	
LD_data_1$Rep <- as.factor(LD_data_1$Rep)	
r2_plot_1 <- ggplot(LD_data_1, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_1$Rep)) , sep = ""),
		title="Selfing rate = 1") + 
	xlim(0,maxd) + 
	ylim(0,1) 
                     
r2_plot_all <- (r2_plot_0 + r2_plot_02 + r2_plot_04) / (r2_plot_06 + r2_plot_08 + r2_plot_095) / (r2_plot_099 + r2_plot_1 + plot_spacer())  & theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=20, hjust=0.5,),
    	      plot.subtitle=element_text(size=20, hjust=0.5),
	      plot.caption=element_text(size=20, hjust=0.5),
	      axis.title.y=element_text(size=30),
	      axis.title.x=element_text(size=30),
	      
	      axis.text.x=element_text(size=30),
	      axis.text.y=element_text(size=30),
	      
	      legend.position="none") 
	      
r2_plot_all <- r2_plot_all + plot_annotation(theme = theme(plot.caption = element_text(size=20, hjust=0.5)))

ggsave(filename = "LD_r2_nonconstant_selfing_all_rates.png",
	plot = r2_plot_all,
	dpi = 300,
	width=18,
	height=25)
		

print(warnings())
