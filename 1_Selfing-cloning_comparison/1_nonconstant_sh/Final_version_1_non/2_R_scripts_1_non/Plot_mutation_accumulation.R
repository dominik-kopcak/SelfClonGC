library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)

replicate_number=20
colours <- c("Selfing"="darkolivegreen2","Asexuality"="darkolivegreen")

Selfing_recessive_load_all <- read.csv("Data_selfing_recessive_load.csv")
Selfing_additive_load_all <- read.csv("Data_selfing_additive_load.csv")
Selfing_relative_homozygosity_all <- read.csv("Data_selfing_relative_homozygosity.csv")

Cloning_recessive_load_all <- read.csv("Data_cloning_recessive_load.csv")
Cloning_additive_load_all <- read.csv("Data_cloning_additive_load.csv")
Cloning_relative_homozygosity_all <- read.csv("Data_cloning_relative_homozygosity.csv")

filename_rec_selfing <- Selfing_recessive_load_all
filename_add_selfing <- Selfing_additive_load_all 
filename_rel_selfing <- Selfing_relative_homozygosity_all 

Selfing_rec_max <- filename_rec_selfing$Expected_recessive_load+1.96*filename_rec_selfing$SD_recessive_load/sqrt(replicate_number)
Selfing_rec_min <- filename_rec_selfing$Expected_recessive_load-1.96*filename_rec_selfing$SD_recessive_load/sqrt(replicate_number)

Selfing_add_max <- filename_add_selfing$Expected_additive_load+1.96*filename_add_selfing$SD_additive_load/sqrt(replicate_number)
Selfing_add_min <- filename_add_selfing$Expected_additive_load-1.96*filename_add_selfing$SD_additive_load/sqrt(replicate_number)

Selfing_rel_max <- filename_rel_selfing$Expected_relative_homozygosity+1.96*filename_rel_selfing$SD_relative_homozygosity/sqrt(replicate_number)
Selfing_rel_min <- filename_rel_selfing$Expected_relative_homozygosity-1.96*filename_rel_selfing$SD_relative_homozygosity/sqrt(replicate_number)

filename_rec_cloning <- Cloning_recessive_load_all
filename_add_cloning <- Cloning_additive_load_all 
filename_rel_cloning <- Cloning_relative_homozygosity_all 

Cloning_rec_max <- filename_rec_cloning$Expected_recessive_load+1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)
Cloning_rec_min <- filename_rec_cloning$Expected_recessive_load-1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)

Cloning_add_max <- filename_add_cloning$Expected_additive_load+1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)
Cloning_add_min <- filename_add_cloning$Expected_additive_load-1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)

Cloning_rel_max <- filename_rel_cloning$Expected_relative_homozygosity+1.96*filename_rel_selfing$SD_relative_homozygosity/sqrt(replicate_number)
Cloning_rel_min <- filename_rel_cloning$Expected_relative_homozygosity-1.96*filename_rel_selfing$SD_relative_homozygosity/sqrt(replicate_number)


Plot_recessive <- ggplot()+
	geom_point(data = filename_rec_selfing, aes(x = Uniparenting_rate, y = Expected_recessive_load/1000, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data = filename_rec_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_rec_max/1000, 
			    ymin=Selfing_rec_min/1000), color="darkolivegreen2", size=1)+
 
 	geom_point(data=filename_rec_cloning, aes(x=Uniparenting_rate, y=Expected_recessive_load/1000, colour="Asexuality"), size=3, shape=15)+
	geom_linerange(data=filename_rec_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_rec_max/1000, 
			    ymin=Cloning_rec_min/1000), color="darkolivegreen", size=1)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Homozygous genotypes\n(x 1000)",	     
	     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = ", replicate_number,", sample size = 50, run for 60 000 generations", sep=""),
	     colour="Reproduction: ")+

	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=10, hjust=0.5),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"), 
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_text(size=15),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position=c(0.5, 0.4),
	      legend.key = element_rect(fill = "white"),
	      legend.box.background = element_rect(color="black", size=2))+
	      
	guides(colour = guide_legend(override.aes = list(size = 8)))+
	      
	scale_colour_manual(values = colours)+
	scale_x_continuous(breaks=seq(0, 1, 0.2))
	
Plot_additive <- ggplot()+
	geom_point(data=filename_add_selfing, aes(x=Uniparenting_rate, y=Expected_additive_load/1000), color = "darkolivegreen2", size=3, shape=15)+
	geom_linerange(data=filename_add_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max/1000, 
			    ymin=Selfing_add_min/1000), color="darkolivegreen2", size=1, 
			key_glyph = draw_key_rect)+
	
	geom_point(data=filename_add_cloning, aes(x=Uniparenting_rate, y=Expected_additive_load/1000), color="darkolivegreen", size=3, shape=15)+
	geom_linerange(data=filename_add_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_add_max/1000, 
			    ymin=Cloning_add_min/1000), color="darkolivegreen", size=1, 
			key_glyph = draw_key_rect)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Total mutation count\n(x 1000)",
	     title="Accumulation of Mutations\nUnder Uniparental Reproduction")+

	     
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=20, hjust=0.5, face="bold"),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"),
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=0),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="none")+
      
	scale_colour_manual(values = colours)+
	scale_x_continuous(breaks=seq(0, 1, 0.2))
                
		   
Plot_additive_2 <- ggplot()+
	geom_point(data=filename_add_selfing, aes(x=Uniparenting_rate, y=Expected_additive_load), color = "darkolivegreen2", size=3, shape=15)+
	geom_linerange(data=filename_add_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max, 
			    ymin=Selfing_add_min), color="darkolivegreen2", size=1, 
			key_glyph = draw_key_rect)+
	
	geom_point(data=filename_add_cloning, aes(x=Uniparenting_rate, y=Expected_additive_load), color="darkolivegreen", size=3, shape=15)+
	geom_linerange(data=filename_add_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_add_max, 
			    ymin=Cloning_add_min), color="darkolivegreen", size=1, 
			key_glyph = draw_key_rect)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Total mutation count",
	     title="Accumulation of Mutations\nUnder Uniparental Reproduction")+

	     
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=20, hjust=0.5, face="bold"),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"),
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=0),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="none")+
      
	scale_colour_manual(values = colours)+
	scale_x_continuous(breaks=seq(0, 1, 0.2))+	
	scale_y_log10(breaks = 10**c(4,5),
                labels = trans_format("log10", math_format(10^.x)))
                
                
Plot_relative <- ggplot()+
	geom_point(data = filename_rel_selfing, aes(x = Uniparenting_rate, y = Expected_relative_homozygosity, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data = filename_rel_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_rel_max, 
			    ymin=Selfing_rel_min), color="darkolivegreen2", size=1)+
 
 	geom_point(data=filename_rel_cloning, aes(x=Uniparenting_rate, y=Expected_relative_homozygosity, colour="Asexuality"), size=3, shape=15)+
	geom_linerange(data=filename_rel_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_rel_max, 
			    ymin=Cloning_rel_min), color="darkolivegreen", size=1)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Relative homozygosity",	     
	     title="Accumulation of Mutations\nUnder Uniparental Reproduction",
	     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = ", replicate_number,", sample size = 50, run for 60 000 generations", sep=""),
	     colour="Reproduction: ")+

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
	      
	      legend.position=c(0.5, 0.85),
	      legend.key = element_rect(fill = "white"),
	      legend.box.background = element_rect(color="black", size=2))+
	      
	guides(colour = guide_legend(override.aes = list(size = 8)))+
	      
	scale_colour_manual(values = colours)+
	scale_x_continuous(breaks=seq(0, 1, 0.2))

Plot <- Plot_additive/Plot_recessive
Plot_2 <- Plot_additive_2/Plot_recessive

plot_name <- paste("Figure_Mutation_accumulation_selfing_vs_cloning_nonconstant_sh.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi=600)
	
plot_name_2 <- paste("Figure_Mutation_accumulation_selfing_vs_cloning_nonconstant_sh_v2.png",sep="")
ggsave(filename=plot_name_2,
	plot=Plot_2,
	dpi=600)

plot_name_rel <- paste("Figure_Mutation_accumulation_relative_homozygosity_selfing_vs_cloning_nonconstant_sh.png",sep="")
ggsave(filename=plot_name_rel,
	plot=Plot_relative,
	dpi=400)
	
