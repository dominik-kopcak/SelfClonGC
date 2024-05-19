library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)

replicate_number=10

GC_recessive_load_all <- read.csv("Data_GC_recessive_load_nonconstant_sh.csv")
GC_additive_load_all <- read.csv("Data_GC_additive_load_nonconstant_sh.csv")

filename_rec_GC <- GC_recessive_load_all
filename_add_GC <- GC_additive_load_all 

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
	
	labs(x="Rate of gene conversion",
	     y="Homozygous genotypes",
	     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 4 000 bp,\nnumber of replicates = ", replicate_number,", sample size = 50, run for 60 000 generations", sep=""))+
	
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

	scale_y_continuous(limits=c(-2, 3500))+
	
	scale_x_log10(breaks=c(10**(seq(-5, -11, -1))),
                labels = trans_format("log10", math_format(10^.x)))
		   
Plot_additive <- ggplot()+
	
	geom_point(data=filename_add_GC, aes(x=GC_rate, y=Expected_additive_load/1000), size=3, shape=15)+
	geom_linerange(data=filename_add_GC, 
			aes(x=GC_rate, 
			    ymax=GC_add_max/1000, 
			    ymin=GC_add_min/1000), size=1)+
	
	labs(x="Rate of gene conversion",
	     y="Total mutation count\n(x 1000)",
	     title="Accumulation of Mutations\nUnder Asexual Reproduction with GC")+
      
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

  	scale_x_log10(labels = trans_format("log10"))
	
Plot <- Plot_additive/Plot_recessive

plot_name <- paste("Figure_Mutation_accumulation_nonconstant_sh.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi=600)
	
