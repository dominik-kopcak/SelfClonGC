library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)
library(tidyverse)

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/6_Evolving_GC/6_constant_sh/4_Data_2_con")

replicate_number=20

GC_recessive_load <- read.csv("Data_GC_recessive_load_WithEvol.csv")
GC_additive_load <- read.csv("Data_GC_additive_load_WithEvol.csv")
GC_relative_homozygosity <- read.csv("Data_GC_relative_homozygosity_WithEvol.csv")

GC_recessive_load$GC_rate <- as.factor(GC_recessive_load$GC_rate)
GC_additive_load$GC_rate <- as.factor(GC_additive_load$GC_rate)
GC_relative_homozygosity$GC_rate <- as.factor(GC_relative_homozygosity$GC_rate)

GC_recessive_load$GC_var <- as.factor(GC_recessive_load$GC_var)
GC_additive_load$GC_var <- as.factor(GC_additive_load$GC_var)
GC_relative_homozygosity$GC_var <- as.factor(GC_relative_homozygosity$GC_var)

for (h_coefficient in c(0.2)) {

filename_rec_GC <- GC_recessive_load %>% filter(Dominance_coefficient == h_coefficient)
filename_add_GC <- GC_additive_load %>% filter(Dominance_coefficient == h_coefficient)
filename_rel_GC <- GC_relative_homozygosity %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_GC <- filename_rec_GC %>% mutate(GC_rec_max=Expected_recessive_load+1.96*SD_recessive_load/sqrt(replicate_number),GC_rec_min=Expected_recessive_load-1.96*SD_recessive_load/sqrt(replicate_number))

filename_add_GC <- filename_add_GC %>% mutate(GC_add_max=Expected_additive_load+1.96*SD_additive_load/sqrt(replicate_number),GC_add_min=Expected_additive_load-1.96*SD_additive_load/sqrt(replicate_number))

filename_rel_GC <- filename_rel_GC %>% mutate(GC_rel_max=Expected_relative_homozygosity+1.96*SD_relative_homozygosity/sqrt(replicate_number),GC_rel_min=Expected_relative_homozygosity-1.96*SD_relative_homozygosity/sqrt(replicate_number))

Plot_recessive <- ggplot(data=filename_rec_GC, aes(x=Cycle, group=GC_var))+
 	geom_point(data=filename_rec_GC, aes(y=Expected_recessive_load, colour=GC_rate, shape=GC_var), size=3)+
	geom_linerange(data=filename_rec_GC, 
			aes(x=Cycle, 
			    ymax=GC_rec_max, 
			    ymin=GC_rec_min,colour=GC_rate),size=1)+
		scale_colour_viridis_d(breaks=unique(filename_rec_GC$GC_rate), labels=c(expression(4 %*% 10 ^ -7), expression(4 %*% 10 ^ -4), expression(4 %*% 10 ^ -2)), direction=-1) + 
		scale_shape_manual(breaks=unique(filename_rec_GC$GC_var), values=c(16,17), name = "Standard deviation in mitotic\ngene conversion mutation rates", labels=c(expression(1 %*% 10 ^ -8), expression(1 %*% 10 ^ -7))) +
		guides(colour = guide_legend(title = "Starting gene conversion\nrate per site")) +
		
	labs(y="Homozygous genotypes",
	     x="Time")+
	     
	     
	     
	 theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=20, hjust=0.5),
	      axis.title.y=element_text(size=30),
	      axis.title.x=element_text(size=30),
	      
	      axis.text.x=element_text(size=35),
	      axis.text.y=element_text(size=35),
	      
	      legend.key = element_blank(),
    	   legend.title = element_text(size=20),
    	   legend.text = element_text(size=20),
    	   legend.key.size = unit(1.5, "cm"))
	      		      
#	  scale_x_log10(breaks = c(10**seq(-8, -2, 1)),labels = trans_format("log10", math_format(10^.x)))+
#     scale_y_continuous(breaks=c(0,250,500,750,1000))                   
	         
Plot_additive <- ggplot(data=filename_add_GC,aes(x=Cycle, group=GC_var))+
	geom_point(data=filename_add_GC, aes(y=Expected_additive_load/1000, colour=GC_rate, shape=GC_var), size=3)+
	geom_linerange(data=filename_add_GC, 
			aes(ymax=GC_add_max/1000, 
			    ymin=GC_add_min/1000, colour=GC_rate), size=1)+
	
	scale_colour_viridis_d(guide = "none", direction=-1) + 
	scale_shape_manual(breaks=unique(filename_rec_GC$GC_var), values=c(16,17), guide="none") +
					    
	labs(x="Time",
	     y="Total mutation count\n(x 1000)",
	     title="Accumulation of Mutations\nUnder Asexual Reproduction with GC")+
	
	
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      

	      plot.title=element_text(size=40, hjust=0.5, face="bold"),
	      axis.title.y=element_text(size=30),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_blank(),
	      axis.text.y=element_text(size=35))


Plot_relative <- ggplot(data=filename_rel_GC, aes(x=Cycle, group=GC_var)) +
 	geom_point(data=filename_rel_GC, aes(y=Expected_relative_homozygosity, colour=GC_rate, shape=GC_var), size=3)+
	geom_linerange(data=filename_rel_GC, 
			aes(x=Cycle, 
			    ymax=GC_rel_max, 
			    ymin=GC_rel_min, colour=GC_rate), size=1)+
			    
		scale_colour_viridis_d(breaks=unique(filename_rel_GC$GC_rate), labels=c(expression(4 %*% 10 ^ -7), expression(4 %*% 10 ^ -4), expression(4 %*% 10 ^ -2)), direction=-1) + 
		scale_shape_manual(breaks=unique(filename_rel_GC$GC_var), values=c(16,17), name = "Standard deviation in mitotic\ngene conversion mutation rates", labels=c(expression(1 %*% 10 ^ -8), expression(1 %*% 10 ^ -7))) +
		guides(colour = guide_legend(title = "Starting gene conversion\nrate per site")) +
	
	labs(y="Relative homozygosity",
	     x="Time",
	     title="Accumulation of Mutations\nUnder Asexual Reproduction with GC")+
	     
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
	      
	      legend.key = element_blank(),
	    	   legend.title = element_text(size=20),
	    	   legend.text = element_text(size=20),
	    	   legend.key.size = unit(1.5, "cm")) +
	      		      		      
		   scale_y_continuous(limits = c(0,1))
          
  	
Plot <- Plot_additive/Plot_recessive

plot_name <- paste("Figure_Mutation_accumulation_GC_", h_coefficient, "_WithEvol.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi = 300,
	width=14,
	height=12)
	
plot_name_2 <- paste("Figure_Mutation_accumulation_GC_relative_homozygosity_", h_coefficient, "_WithEvol.png",sep="")
ggsave(filename=plot_name_2,
	plot=Plot_relative,
	dpi = 300,
	width=14,
	height=12)
}
