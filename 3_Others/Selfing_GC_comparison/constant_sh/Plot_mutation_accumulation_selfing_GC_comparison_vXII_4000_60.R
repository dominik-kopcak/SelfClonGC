library(ggplot2)
library(patchwork)
library(dplyr)

replicate_number=100
colours <- c("Selfing"="black", "Cloning with GC rate 1E-5"="brown", "Cloning with GC rate 1E-6"="red", "Cloning with GC rate 1E-7"="orange", "Cloning with GC rate 1E-8"="gold","Cloning with GC rate 1E-9"="yellow")

# ADJUST PATHS AS NEEDED

Selfing_recessive_load_all <- read.csv("Data_selfing_recessive_load_full_60000.csv")
Selfing_additive_load_all <- read.csv("Data_selfing_additive_load_full_60000.csv")

Cloning_recessive_load_all <- read.csv("Data_GC_recessive_load_vXII_full_60000.csv")
Cloning_additive_load_all <- read.csv("Data_GC_additive_load_vXII_full_60000.csv")

print(Selfing_additive_load_all[,c(4,5)])
print(Cloning_additive_load_all[,c(4,5)])

Cloning_recessive_load_5 <- Cloning_recessive_load_all %>%
	filter(GC_rate == 10**(-5))
Cloning_additive_load_5 <- Cloning_additive_load_all %>%
	filter(GC_rate == 10**(-5))
	
Cloning_recessive_load_6 <- Cloning_recessive_load_all %>%
	filter(GC_rate == 10**(-6))
Cloning_additive_load_6 <- Cloning_additive_load_all %>%
	filter(GC_rate == 10**(-6))

Cloning_recessive_load_7 <- Cloning_recessive_load_all %>%
	filter(GC_rate == 10**(-7))
Cloning_additive_load_7 <- Cloning_additive_load_all %>%
	filter(GC_rate == 10**(-7))

Cloning_recessive_load_8 <- Cloning_recessive_load_all %>%
	filter(GC_rate == 10**(-8))
Cloning_additive_load_8 <- Cloning_additive_load_all %>%
	filter(GC_rate == 10**(-8))

Cloning_recessive_load_9 <- Cloning_recessive_load_all %>%
	filter(GC_rate == 10**(-9))
Cloning_additive_load_9 <- Cloning_additive_load_all %>%
	filter(GC_rate == 10**(-9))
	
for (h_coefficient in c(0.2)) {

filename_rec_selfing <- Selfing_recessive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_add_selfing <- Selfing_additive_load_all %>% filter(Dominance_coefficient == h_coefficient)

Selfing_rec_max <- filename_rec_selfing$Expected_recessive_load+1.96*filename_rec_selfing$SD_recessive_load/sqrt(replicate_number)
Selfing_rec_min <- filename_rec_selfing$Expected_recessive_load-1.96*filename_rec_selfing$SD_recessive_load/sqrt(replicate_number)

Selfing_add_max <- filename_add_selfing$Expected_additive_load+1.96*filename_add_selfing$SD_additive_load/sqrt(replicate_number)
Selfing_add_min <- filename_add_selfing$Expected_additive_load-1.96*filename_add_selfing$SD_additive_load/sqrt(replicate_number)

filename_rec_cloning_5 <- Cloning_recessive_load_5 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_5 <- Cloning_additive_load_5 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_6 <- Cloning_recessive_load_6 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_6 <- Cloning_additive_load_6 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_7 <- Cloning_recessive_load_7 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_7 <- Cloning_additive_load_7 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_8 <- Cloning_recessive_load_8 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_8 <- Cloning_additive_load_8 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_9 <- Cloning_recessive_load_9 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_9 <- Cloning_additive_load_9 %>% filter(Dominance_coefficient == h_coefficient)

Plot_recessive <- ggplot()+

	geom_segment(data=filename_rec_cloning_5, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "brown")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_rec_cloning_5$Expected_recessive_load+1.96*filename_rec_cloning_5$SD_recessive_load/sqrt(replicate_number)), 
			    ymin=(filename_rec_cloning_5$Expected_recessive_load-1.96*filename_rec_cloning_5$SD_recessive_load/sqrt(replicate_number))), fill ="brown",  alpha=0.1)+
	
	geom_segment(data=filename_rec_cloning_6, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "red")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_rec_cloning_6$Expected_recessive_load+1.96*filename_rec_cloning_6$SD_recessive_load/sqrt(replicate_number)), 
			    ymin=(filename_rec_cloning_6$Expected_recessive_load-1.96*filename_rec_cloning_6$SD_recessive_load/sqrt(replicate_number))), fill ="red",  alpha=0.1)+
	
	geom_segment(data=filename_rec_cloning_7, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "orange")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_rec_cloning_7$Expected_recessive_load+1.96*filename_rec_cloning_7$SD_recessive_load/sqrt(replicate_number)), 
			    ymin=(filename_rec_cloning_7$Expected_recessive_load-1.96*filename_rec_cloning_7$SD_recessive_load/sqrt(replicate_number))), fill ="orange",  alpha=0.1)+
	
	geom_segment(data=filename_rec_cloning_8, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "gold")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_rec_cloning_8$Expected_recessive_load+1.96*filename_rec_cloning_8$SD_recessive_load/sqrt(replicate_number)), 
			    ymin=(filename_rec_cloning_8$Expected_recessive_load-1.96*filename_rec_cloning_8$SD_recessive_load/sqrt(replicate_number))), fill ="gold",  alpha=0.1)+
	
	geom_segment(data=filename_rec_cloning_9, aes(x=0, y=Expected_recessive_load, xend=1, yend=Expected_recessive_load), colour = "yellow")+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_rec_cloning_9$Expected_recessive_load+1.96*filename_rec_cloning_9$SD_recessive_load/sqrt(replicate_number)), 
			    ymin=(filename_rec_cloning_9$Expected_recessive_load-1.96*filename_rec_cloning_9$SD_recessive_load/sqrt(replicate_number))), fill ="yellow",  alpha=0.1)+
	
	geom_point(data = filename_rec_selfing, aes(x = Uniparenting_rate, y = Expected_recessive_load), color = "black", size=3, shape=15)+
	geom_linerange(data = filename_rec_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_rec_max, 
			    ymin=Selfing_rec_min), color="black", size=1)+
	
	labs(x="Rate of selfing",
	     y="Homozygous\ngenotypes")+
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=20, hjust=0.5),
	      axis.title.y=element_text(size=35),
	      axis.title.x=element_text(size=35),
	      
	      axis.text.x=element_text(size=35),
	      axis.text.y=element_text(size=35),
	     
	      legend.position="none")+
	      
	scale_colour_manual(values = colours) +
	scale_x_continuous(breaks=seq(0, 1, 0.2))

			    	   
Plot_additive <- ggplot()+
	
	geom_segment(data=filename_add_cloning_5, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-5"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_5$Expected_additive_load+1.96*filename_add_cloning_5$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_5$Expected_additive_load-1.96*filename_add_cloning_5$SD_additive_load/sqrt(replicate_number))/1000), fill ="brown",  alpha=0.1)+
	
	geom_segment(data=filename_add_cloning_6, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-6"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_6$Expected_additive_load+1.96*filename_add_cloning_6$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_6$Expected_additive_load-1.96*filename_add_cloning_6$SD_additive_load/sqrt(replicate_number))/1000), fill ="red",  alpha=0.1)+
	
	geom_segment(data=filename_add_cloning_7, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-7"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_7$Expected_additive_load+1.96*filename_add_cloning_7$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_7$Expected_additive_load-1.96*filename_add_cloning_7$SD_additive_load/sqrt(replicate_number))/1000), fill ="orange",  alpha=0.1)+
	
	geom_segment(data=filename_add_cloning_8, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-8"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_8$Expected_additive_load+1.96*filename_add_cloning_8$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_8$Expected_additive_load-1.96*filename_add_cloning_8$SD_additive_load/sqrt(replicate_number))/1000), fill ="gold",  alpha=0.1)+
			    
	geom_segment(data=filename_add_cloning_9, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-9"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_9$Expected_additive_load+1.96*filename_add_cloning_9$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_9$Expected_additive_load-1.96*filename_add_cloning_9$SD_additive_load/sqrt(replicate_number))/1000), fill ="yellow",  alpha=0.1)+
	
	geom_point(data=filename_add_selfing, aes(x=Uniparenting_rate, y=Expected_additive_load/1000, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data=filename_add_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max/1000, 
			    ymin=Selfing_add_min/1000), color="black", size=1, 
			key_glyph = "rect")+
	
	labs(y="Total mutation count\n(x1000)",
	     x="Rate of uniparental reproduction",
	     colour="Reproduction: ")+
	 
	guides(colour = guide_legend(override.aes = list(size = 10)))+
	
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=40, hjust=0.5, face="bold"),
	      axis.title.y=element_text(size=35),
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

	geom_segment(data=filename_add_cloning_5, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-5"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_5$Expected_additive_load+1.96*filename_add_cloning_5$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_5$Expected_additive_load-1.96*filename_add_cloning_5$SD_additive_load/sqrt(replicate_number))/1000), fill ="brown",  alpha=0.1)+


	geom_segment(data=filename_add_cloning_6, aes(x=0, y=Expected_additive_load/1000, xend=1, yend=Expected_additive_load/1000, colour = "Cloning with GC rate 1E-6"))+
	geom_ribbon(aes(x=c(0,1),
			    ymax=(filename_add_cloning_6$Expected_additive_load+1.96*filename_add_cloning_6$SD_additive_load/sqrt(replicate_number))/1000, 
			    ymin=(filename_add_cloning_6$Expected_additive_load-1.96*filename_add_cloning_6$SD_additive_load/sqrt(replicate_number))/1000), fill ="red",  alpha=0.1)+

	
	geom_point(data=filename_add_selfing, aes(x=Uniparenting_rate, y=Expected_additive_load/1000, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data=filename_add_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max/1000, 
			    ymin=Selfing_add_min/1000), color="black", size=1, 
			key_glyph = "rect")+
	
	labs(y="Total mutation count\n(x1000)",
	     x="Rate of uniparental reproduction",
	     colour="Reproduction: ")+
	
	guides(colour = guide_legend(override.aes = list(size = 10)))+
	
	 theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=40, hjust=0.5, face="bold"),
	      axis.title.y=element_text(size=35),
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
	
	
Plot <- Plot_additive/Plot_recessive
Plot_2 <- Plot_additive_2/Plot_recessive

plot_name <- paste("Figure_Mutation_accumulation_selfing_vs_GC_", h_coefficient, "_vXII_4000_60.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi = 300,
	width = 14,
	height = 12)
	
plot_name_2 <- paste("Figure_Mutation_accumulation_selfing_vs_GC_", h_coefficient, "_vXII_4000_60_v2.png",sep="")
ggsave(filename=plot_name_2,
	plot=Plot_2,
	dpi = 300,
	width = 14,
	height = 12)
	
}
