library(ggplot2)
library(patchwork)
library(dplyr)

replicate_number_GC=10
replicate_number_sel=20
colours <- c("Selfing"="black", "Cloning with GC rate 1E-5"="brown", "Cloning with GC rate 1E-6"="red", "Cloning with GC rate 1E-7"="orange", "Cloning with GC rate 1E-8"="gold","Cloning with GC rate 1E-9"="yellow")

# ADJUST PATHS AS NEEDED

Selfing_recessive_load_all <- read.csv("Data_selfing_recessive_load.csv")
Selfing_additive_load_all <- read.csv("Data_selfing_additive_load.csv")

Cloning_recessive_load_all <- read.csv("Data_GC_recessive_load_nonconstant_sh.csv")
Cloning_additive_load_all <- read.csv("Data_GC_additive_load_nonconstant_sh.csv")

print(Selfing_additive_load_all[,c(2,3)])
print(Cloning_additive_load_all[,c(2,3)])

Cloning_recessive_load_5 <- Cloning_recessive_load_all %>% filter(GC_rate == 10**(-5))
Cloning_additive_load_5 <- Cloning_additive_load_all %>% filter(GC_rate == 10**(-5))
	
Cloning_recessive_load_6 <- Cloning_recessive_load_all %>% filter(GC_rate == 10**(-6))
Cloning_additive_load_6 <- Cloning_additive_load_all %>% filter(GC_rate == 10**(-6))

Cloning_recessive_load_7 <- Cloning_recessive_load_all %>% filter(GC_rate == 10**(-7))
Cloning_additive_load_7 <- Cloning_additive_load_all %>% filter(GC_rate == 10**(-7))

Cloning_recessive_load_8 <- Cloning_recessive_load_all %>% filter(GC_rate == 10**(-8))
Cloning_additive_load_8 <- Cloning_additive_load_all %>% filter(GC_rate == 10**(-8))

Cloning_recessive_load_9 <- Cloning_recessive_load_all %>% filter(GC_rate == 10**(-9))
Cloning_additive_load_9 <- Cloning_additive_load_all %>% filter(GC_rate == 10**(-9))

Selfing_rec_max <- Selfing_recessive_load_all$Expected_recessive_load+1.96*Selfing_recessive_load_all$SD_recessive_load/sqrt(replicate_number_sel)
Selfing_rec_min <- Selfing_recessive_load_all$Expected_recessive_load-1.96*Selfing_recessive_load_all$SD_recessive_load/sqrt(replicate_number_sel)

Selfing_add_max <- Selfing_additive_load_all$Expected_additive_load+1.96*Selfing_additive_load_all$SD_additive_load/sqrt(replicate_number_sel)
Selfing_add_min <- Selfing_additive_load_all$Expected_additive_load-1.96*Selfing_additive_load_all$SD_additive_load/sqrt(replicate_number_sel)

Plot_recessive <- ggplot()+

	geom_segment(data=Cloning_recessive_load_5, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "brown")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_5$Expected_recessive_load+1.96*Cloning_recessive_load_5$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_5$Expected_recessive_load-1.96*Cloning_recessive_load_5$SD_recessive_load/sqrt(replicate_number_GC))), fill ="brown",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_6, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "red")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_6$Expected_recessive_load+1.96*Cloning_recessive_load_6$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_6$Expected_recessive_load-1.96*Cloning_recessive_load_6$SD_recessive_load/sqrt(replicate_number_GC))), fill ="red",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_7, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "orange")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_7$Expected_recessive_load+1.96*Cloning_recessive_load_7$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_7$Expected_recessive_load-1.96*Cloning_recessive_load_7$SD_recessive_load/sqrt(replicate_number_GC))), fill ="orange",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_8, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "gold")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_8$Expected_recessive_load+1.96*Cloning_recessive_load_8$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_8$Expected_recessive_load-1.96*Cloning_recessive_load_8$SD_recessive_load/sqrt(replicate_number_GC))), fill ="gold",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_9, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "yellow")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_9$Expected_recessive_load+1.96*Cloning_recessive_load_9$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_9$Expected_recessive_load-1.96*Cloning_recessive_load_9$SD_recessive_load/sqrt(replicate_number_GC))), fill ="yellow",  alpha=0.1)+
	
	geom_point(data = Selfing_recessive_load_all, aes(x = Uniparenting_rate, y = Expected_recessive_load), color = "black", size=3, shape=15)+
	geom_linerange(data = Selfing_recessive_load_all, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_rec_max, 
			    ymin=Selfing_rec_min), color="black", size=1)+
	
	labs(x="Rate of selfing",
	     y="Homozygous\ngenotypes",
	     caption="for both WF and non-WF: sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, sample size = 50, run for 60 000 generations\nfor WF only: recombination rate = 4E-8, number of replicates = 20,\nfor non-WF only: mean GC tract length = 4 000 bp, number of replicates = 10")+
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=20, hjust=0.5),
	      axis.title.y=element_text(size=32),
	      axis.title.x=element_text(size=35),
	      
	      axis.text.x=element_text(size=35),
	      axis.text.y=element_text(size=35),
	     
	      legend.position="none")+
	
	guides(colour = guide_legend(override.aes = list(size = 10)))+
	      
	scale_colour_manual(values = colours) +
	scale_x_continuous(breaks=seq(0, 1, 0.2))
	
Plot_recessive_2 <- ggplot()+

	geom_segment(data=Cloning_recessive_load_5, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load, colour = "Cloning with GC rate 1E-5"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_5$Expected_recessive_load+1.96*Cloning_recessive_load_5$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_5$Expected_recessive_load-1.96*Cloning_recessive_load_5$SD_recessive_load/sqrt(replicate_number_GC))), fill ="brown",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_6, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load, colour = "Cloning with GC rate 1E-6"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_6$Expected_recessive_load+1.96*Cloning_recessive_load_6$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_6$Expected_recessive_load-1.96*Cloning_recessive_load_6$SD_recessive_load/sqrt(replicate_number_GC))), fill ="red",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_7, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load, colour = "Cloning with GC rate 1E-7"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_7$Expected_recessive_load+1.96*Cloning_recessive_load_7$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_7$Expected_recessive_load-1.96*Cloning_recessive_load_7$SD_recessive_load/sqrt(replicate_number_GC))), fill ="orange",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_8, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load, colour = "Cloning with GC rate 1E-8"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_8$Expected_recessive_load+1.96*Cloning_recessive_load_8$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_8$Expected_recessive_load-1.96*Cloning_recessive_load_8$SD_recessive_load/sqrt(replicate_number_GC))), fill ="gold",  alpha=0.1)+
	
	geom_segment(data=Cloning_recessive_load_9, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load, colour = "Cloning with GC rate 1E-9"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_recessive_load_9$Expected_recessive_load+1.96*Cloning_recessive_load_9$SD_recessive_load/sqrt(replicate_number_GC)), 
			    ymin=(Cloning_recessive_load_9$Expected_recessive_load-1.96*Cloning_recessive_load_9$SD_recessive_load/sqrt(replicate_number_GC))), fill ="yellow",  alpha=0.1)+
	
	geom_point(data = Selfing_recessive_load_all, aes(x = Uniparenting_rate, y = Expected_recessive_load, colour = "Selfing"), size=3, shape=15)+
	geom_linerange(data = Selfing_recessive_load_all, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_rec_max, 
			    ymin=Selfing_rec_min), color="black", size=1)+
	
	labs(x="Rate of selfing",
	     y="Homozygous\ngenotypes",
	     caption="for both WF and non-WF: sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, sample size = 50, run for 60 000 generations\nfor WF only: recombination rate = 4E-8, number of replicates = 20,\nfor non-WF only: mean GC tract length = 4 000 bp, number of replicates = 10",
	     colour="Reproduction: ")+
	guides(colour = guide_legend(override.aes = list(size = 10)))+
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=20, hjust=0.5),
	      axis.title.y=element_text(size=32),
	      axis.title.x=element_text(size=35),
	      
	      axis.text.x=element_text(size=35),
	      axis.text.y=element_text(size=35),
	      
	      legend.position=c(0.21, 0.41),     
	      legend.background=element_rect(colour="black"),
	      legend.key = element_rect(fill = "white"),
	      legend.title = element_text(size=15),
	      legend.text = element_text(size=15),
	      legend.key.size = unit(1, "cm"))+
	      
	scale_colour_manual(values = colours, labels=c("Selfing                                               ", 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -2)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -3)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -4)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -5)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -6)))) +
	scale_x_continuous(breaks=seq(0, 1, 0.2))

			    	   
Plot_additive <- ggplot()+
	
	geom_segment(data=Cloning_additive_load_5, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-5"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_5$Expected_additive_load+1.96*Cloning_additive_load_5$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_5$Expected_additive_load-1.96*Cloning_additive_load_5$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="brown",  alpha=0.1)+
	
	geom_segment(data=Cloning_additive_load_6, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-6"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_6$Expected_additive_load+1.96*Cloning_additive_load_6$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_6$Expected_additive_load-1.96*Cloning_additive_load_6$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="red",  alpha=0.1)+
	
	geom_segment(data=Cloning_additive_load_7, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-7"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_7$Expected_additive_load+1.96*Cloning_additive_load_7$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_7$Expected_additive_load-1.96*Cloning_additive_load_7$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="orange",  alpha=0.1)+
	
	geom_segment(data=Cloning_additive_load_8, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-8"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_8$Expected_additive_load+1.96*Cloning_additive_load_8$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_8$Expected_additive_load-1.96*Cloning_additive_load_8$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="gold",  alpha=0.1)+
			    
	geom_segment(data=Cloning_additive_load_9, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-9"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_9$Expected_additive_load+1.96*Cloning_additive_load_9$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_9$Expected_additive_load-1.96*Cloning_additive_load_9$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="yellow",  alpha=0.1)+
	
	geom_point(data=Selfing_additive_load_all, aes(x=Uniparenting_rate, y=Expected_additive_load/1000, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data=Selfing_additive_load_all, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max/1000, 
			    ymin=Selfing_add_min/1000), color="black", size=1, 
			key_glyph = "rect")+
	
	labs(y="Total mutation count\n(x1000)",
	     x="Rate of uniparental reproduction",
	     title="Accumulation of Mutations\nUnder Selfing and Asexuality with GC\nAnd a Distribution of Fitness Effects",
	     colour="Reproduction: ")+
	guides(colour = guide_legend(override.aes = list(size = 10)))+
	
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=40, hjust=0.5, face="bold"),
	      axis.title.y=element_text(size=32),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=0),
	      axis.text.y=element_text(size=35),
	      
	      legend.position=c(0.79, 0.65),     
	      legend.background=element_rect(colour="black"),
	      legend.key = element_rect(fill = "white"),
	      legend.title = element_text(size=15),
	      legend.text = element_text(size=15),
	      legend.key.size = unit(1, "cm"))+
	      
	scale_colour_manual(values = colours, labels=c("Selfing                                               ", 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -2)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -3)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -4)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -5)), 
							expression(paste("Complete asex. + GC rate ", 4 %*% 10 ^ -6)))) +
	scale_shape_manual(guide = "none")+
	scale_x_continuous(breaks=seq(0, 1, 0.2))
	

Plot_additive_2 <- ggplot()+

	geom_segment(data=Cloning_additive_load_5, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-5"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_5$Expected_additive_load+1.96*Cloning_additive_load_5$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_5$Expected_additive_load-1.96*Cloning_additive_load_5$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="brown",  alpha=0.1)+


	geom_segment(data=Cloning_additive_load_6, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-6"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_6$Expected_additive_load+1.96*Cloning_additive_load_6$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_6$Expected_additive_load-1.96*Cloning_additive_load_6$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="red",  alpha=0.1)+
	
			    
	geom_segment(data=Cloning_additive_load_7, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-7"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(Cloning_additive_load_7$Expected_additive_load+1.96*Cloning_additive_load_7$SD_additive_load/sqrt(replicate_number_GC))/1000, 
			    ymin=(Cloning_additive_load_7$Expected_additive_load-1.96*Cloning_additive_load_7$SD_additive_load/sqrt(replicate_number_GC))/1000), fill ="orange",  alpha=0.1)+

	
	geom_point(data=Selfing_additive_load_all, aes(x=Uniparenting_rate, y=Expected_additive_load/1000, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data=Selfing_additive_load_all, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max/1000, 
			    ymin=Selfing_add_min/1000), color="black", size=1, 
			key_glyph = "rect")+
	
	labs(y="Total mutation count\n(x1000)",
	     x="Rate of uniparental reproduction",
	     title="Accumulation of Mutations\nUnder Selfing and Asexuality with GC\nAnd a Distribution of Fitness Effects",
	     colour="Reproduction: ")+
	guides(colour = guide_legend(override.aes = list(size = 10)))+
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=40, hjust=0.5, face="bold"),
	      axis.title.y=element_text(size=32),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=0),
	      axis.text.y=element_text(size=35),
	      
	      legend.position="none")+
	      
	scale_colour_manual(values = colours) +
	scale_shape_manual(guide = "none")+
	scale_x_continuous(breaks=seq(0, 1, 0.2))
	
	
Plot <- Plot_additive/Plot_recessive
Plot_2 <- Plot_additive_2/Plot_recessive_2

plot_name <- "Figure_Mutation_accumulation_selfing_vs_GC_nonconstant_vXII_4000_60.png"
ggsave(filename=plot_name,
	plot=Plot,
	dpi = 300,
	width = 14,
	height = 12)
	
plot_name_2 <- "Figure_Mutation_accumulation_selfing_vs_GC_nonconstant_vXII_4000_60_v2.png"
ggsave(filename=plot_name_2,
	plot=Plot_2,
	dpi = 300,
	width = 14,
	height = 12)
