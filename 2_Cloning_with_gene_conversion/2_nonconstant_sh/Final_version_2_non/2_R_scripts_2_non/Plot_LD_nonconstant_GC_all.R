#!/usr/bin/env Rscript
library(tidyverse)
library(scales)
library(patchwork)

max_ee <- 100
maxd <- 12.5 	

LD_data_05 <- read.csv("Data_LD__nonconstant_sh_GC_1e-05.csv")
LD_data_06 <- read.csv("Data_LD__nonconstant_sh_GC_1e-06.csv")
LD_data_07 <- read.csv("Data_LD__nonconstant_sh_GC_1e-07.csv")
LD_data_08 <- read.csv("Data_LD__nonconstant_sh_GC_1e-08.csv")
LD_data_09 <- read.csv("Data_LD__nonconstant_sh_GC_1e-09.csv")
LD_data_10 <- read.csv("Data_LD__nonconstant_sh_GC_1e-10.csv")
LD_data_11 <- read.csv("Data_LD__nonconstant_sh_GC_1e-11.csv")

print("---")

# Plotting LD decay (r^2 and Dprime)

# 05
LD_data_05$Rep <- as.factor(LD_data_05$Rep)
r2_plot_05 <- ggplot(LD_data_05, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_05$Rep)) , sep = ""),
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-2))) + 

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
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-3))) + 
	xlim(0,maxd) + 
	ylim(0,1) 

# 07	
LD_data_07$Rep <- as.factor(LD_data_07$Rep)	
r2_plot_07 <- ggplot(LD_data_07, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_07$Rep)) , sep = ""),
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-4))) + 
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
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-5))) + 
	xlim(0,maxd) + 
	ylim(0,1) 
                     
                    
# 09	
LD_data_09$Rep <- as.factor(LD_data_09$Rep)	
r2_plot_09 <- ggplot(LD_data_09, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_09$Rep)) , sep = ""),
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-6))) + 
	xlim(0,maxd) + 
	ylim(0,1) 
	
# 10	
LD_data_10$Rep <- as.factor(LD_data_10$Rep)	
r2_plot_10 <- ggplot(LD_data_10, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_10$Rep)) , sep = ""),
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-7))) + 
	xlim(0,maxd) + 
	ylim(0,1) 
	
# 11	
LD_data_11$Rep <- as.factor(LD_data_11$Rep)	
r2_plot_11 <- ggplot(LD_data_11, aes(x=DIST_MB, y=R.2)) + 
	geom_point(aes(color=Rep),alpha=0.1) + 
	geom_smooth(aes(color=Rep)) + 
	geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
    scale_color_brewer(palette = "RdBu") +     
    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")")), 
    		subtitle=paste("number of replicates plotted = ", length(unique(LD_data_11$Rep)) , sep = ""),
		title=expression(paste("Mean GC rate per site = ", 4%*%10^-8))) + 
	xlim(0,maxd) + 
	ylim(0,1) 
                     
r2_plot_all <- (r2_plot_11 + r2_plot_10 + r2_plot_09) / (r2_plot_08 + r2_plot_07 + r2_plot_06) / (plot_spacer() + r2_plot_05 + plot_spacer())  & theme(panel.grid.major=element_blank(),
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

ggsave(filename = "LD_r2_nonconstant_GC_all_rates.png",
	plot = r2_plot_all,
	dpi = 300,
	width=18,
	height=25)
		

print(warnings())
