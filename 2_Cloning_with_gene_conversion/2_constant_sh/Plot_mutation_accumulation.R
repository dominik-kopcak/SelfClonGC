library(ggplot2)
library(patchwork)
library(dplyr)

replicate_number=100

GC_recessive_load_all <- read.csv("Data_GC_recessive_load.csv")
GC_additive_load_all <- read.csv("Data_GC_additive_load.csv")
GC_fixed_mutations_all <- read.csv("Data_GC_fixed_mutations.csv")

for (h_coefficient in c(0.2, 0.5)) {

filename_rec_GC <- GC_recessive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_add_GC <- GC_additive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_fix_GC <- GC_fixed_mutations_all %>% filter(Dominance_coefficient == h_coefficient)


GC_rec_max <- filename_rec_GC$Expected_recessive_load+1.96*filename_rec_GC$SD_recessive_load/sqrt(replicate_number)
GC_rec_min <- filename_rec_GC$Expected_recessive_load-1.96*filename_rec_GC$SD_recessive_load/sqrt(replicate_number)

GC_add_max <- filename_add_GC$Expected_additive_load+1.96*filename_add_GC$SD_additive_load/sqrt(replicate_number)
GC_add_min <- filename_add_GC$Expected_additive_load-1.96*filename_add_GC$SD_additive_load/sqrt(replicate_number)

Plot_recessive <- ggplot()+
 	geom_point(data=filename_rec_GC, aes(x=GC_rate, y=Expected_recessive_load), size=3, shape=15)+
	geom_linerange(data=filename_rec_GC, 
			aes(x=GC_rate, 
			    ymax=GC_rec_max, 
			    ymin=GC_rec_min), size=1)+
	
	labs(y="Homozygous genotypes",
	     x="Rate of uniparental reproduction",
	     title="Accumulation of mutations\nin completely clonal populations\nwith gene conversion",
	     subtitle="Recessive load")+
	 theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=20, hjust=0.5, face="bold"),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"),
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="none")+
	      
  	scale_x_log10(breaks=c(10**seq(-10, -4, 2)))
		   
Plot_additive <- ggplot()+
	geom_point(data=filename_add_GC, aes(x=GC_rate, y=Expected_additive_load/1000), size=3, shape=15)+
	geom_linerange(data=filename_add_GC, 
			aes(x=GC_rate, 
			    ymax=GC_add_max/1000, 
			    ymin=GC_add_min/1000), size=1, 
			key_glyph = draw_key_rect)+
	
	labs(subtitle="Additive load",
	     x="Rate of gene conversion",
	     y="Total mutation count (x 1000)",
	     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 100 bp\nnumber of replicates = 100, sample size = 50, run for 20 000 generations", sep=""),
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
	      
	      legend.position=c(0.12, 2.1),
	      legend.background=element_rect(colour="black"))+
	      
  	scale_x_log10(breaks=c(10**seq(-10, -4, 1)))
  	
Plot <- Plot_recessive/Plot_additive

plot_name <- paste("Figure_Mutation_accumulation_GC_", h_coefficient, ".png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi=600)
}
