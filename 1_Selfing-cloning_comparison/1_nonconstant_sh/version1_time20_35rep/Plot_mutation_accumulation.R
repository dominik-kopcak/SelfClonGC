library(ggplot2)
library(patchwork)
library(dplyr)

replicate_number=35
colours <- c("Selfing"="darkolivegreen2","Cloning"="darkolivegreen")

Selfing_recessive_load_all <- read.csv("Data_selfing_recessive_load.csv")
Selfing_additive_load_all <- read.csv("Data_selfing_additive_load.csv")
Selfing_fixed_mutations_all <- read.csv("Data_selfing_fixed_mutations.csv")

Cloning_recessive_load_all <- read.csv("Data_cloning_recessive_load.csv")
Cloning_additive_load_all <- read.csv("Data_cloning_additive_load.csv")
Cloning_fixed_mutations_all <- read.csv("Data_cloning_fixed_mutations.csv")


filename_rec_selfing <- Selfing_recessive_load_all
filename_add_selfing <- Selfing_additive_load_all 
filename_fix_selfing <- Selfing_fixed_mutations_all

Selfing_rec_max <- filename_rec_selfing$Expected_recessive_load+1.96*filename_rec_selfing$SD_recessive_load/sqrt(replicate_number)
Selfing_rec_min <- filename_rec_selfing$Expected_recessive_load-1.96*filename_rec_selfing$SD_recessive_load/sqrt(replicate_number)

Selfing_add_max <- filename_add_selfing$Expected_additive_load+1.96*filename_add_selfing$SD_additive_load/sqrt(replicate_number)
Selfing_add_min <- filename_add_selfing$Expected_additive_load-1.96*filename_add_selfing$SD_additive_load/sqrt(replicate_number)


filename_rec_cloning <- Cloning_recessive_load_all
filename_add_cloning <- Cloning_additive_load_all 
filename_fix_cloning <- Cloning_fixed_mutations_all


Cloning_rec_max <- filename_rec_cloning$Expected_recessive_load+1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)
Cloning_rec_min <- filename_rec_cloning$Expected_recessive_load-1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)

Cloning_add_max <- filename_add_cloning$Expected_additive_load+1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)
Cloning_add_min <- filename_add_cloning$Expected_additive_load-1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)


Plot_recessive <- ggplot()+
	geom_point(data = filename_rec_selfing, aes(x = Uniparenting_rate, y = Expected_recessive_load), color = "darkolivegreen2", size=3, shape=15)+
	geom_linerange(data = filename_rec_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_rec_max, 
			    ymin=Selfing_rec_min), color="darkolivegreen2", size=1)+
 
 	geom_point(data=filename_rec_cloning, aes(x=Uniparenting_rate, y=Expected_recessive_load), color="darkolivegreen", size=3, shape=15)+
	geom_linerange(data=filename_rec_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_rec_max, 
			    ymin=Cloning_rec_min), color="darkolivegreen", size=1)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Homozygous genotypes",
	     title="Accumulation of mutations\nin populations with uniparental reproduction",
	     subtitle="Recessive load")+
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
		   
Plot_additive <- ggplot()+
	geom_point(data=filename_add_selfing, aes(x=Uniparenting_rate, y=Expected_additive_load/1000, colour="Selfing"), size=3, shape=15)+
	geom_linerange(data=filename_add_selfing, 
			aes(x=Uniparenting_rate, 
			    ymax=Selfing_add_max/1000, 
			    ymin=Selfing_add_min/1000), color="darkolivegreen2", size=1, 
			key_glyph = draw_key_rect)+
	
	geom_point(data=filename_add_cloning, aes(x=Uniparenting_rate, y=Expected_additive_load/1000, colour="Cloning"), size=3, shape=15)+
	geom_linerange(data=filename_add_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_add_max/1000, 
			    ymin=Cloning_add_min/1000), color="darkolivegreen", size=1, 
			key_glyph = draw_key_rect)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Total mutation count\n(x 1000)",
	     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = ", replicate_number,", sample size = 50, run for 20 000 generations", sep=""),
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
	      
	      legend.position=c(0.12, 0.8),
	      legend.key = element_rect(fill = "white"))+

	guides(colour = guide_legend(override.aes = list(size = 8)))+
	      
	scale_colour_manual(values = colours)+
	scale_x_continuous(breaks=seq(0, 1, 0.2))

Plot <- Plot_recessive/Plot_additive

plot_name <- paste("Figure_Mutation_accumulation_selfing_vs_cloning_nonconstant_sh.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi=600)
	
