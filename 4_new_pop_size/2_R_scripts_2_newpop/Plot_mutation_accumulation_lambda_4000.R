library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)

replicate_number=20

setwd("/mnt/loki/hartfield/AsexMuts/scripts/SelfClonGC/4_new_pop_size/4_plots/")

GC_recessive_load <- read.csv("Data_GC_recessive_load_vXII_full_60000_newpop.csv")
GC_additive_load <- read.csv("Data_GC_additive_load_vXII_full_60000_newpop.csv")
GC_relative_homozygosity <- read.csv("Data_GC_relative_homozygosity_vXII_full_60000_newpop.csv")

for (h_coefficient in c(0.2)) {

filename_rec_GC <- GC_recessive_load %>% filter(Dominance_coefficient == h_coefficient)
filename_add_GC <- GC_additive_load %>% filter(Dominance_coefficient == h_coefficient)
filename_rel_GC <- GC_relative_homozygosity %>% filter(Dominance_coefficient == h_coefficient)

GC_rec_max <- filename_rec_GC$Expected_recessive_load+1.96*filename_rec_GC$SD_recessive_load/sqrt(replicate_number)
GC_rec_min <- filename_rec_GC$Expected_recessive_load-1.96*filename_rec_GC$SD_recessive_load/sqrt(replicate_number)

print(GC_rec_max)

GC_add_max <- filename_add_GC$Expected_additive_load+1.96*filename_add_GC$SD_additive_load/sqrt(replicate_number)
GC_add_min <- filename_add_GC$Expected_additive_load-1.96*filename_add_GC$SD_additive_load/sqrt(replicate_number)

GC_rel_max <- filename_rel_GC$Expected_relative_homozygosity+1.96*filename_rel_GC$SD_relative_homozygosity/sqrt(replicate_number)
GC_rel_min <- filename_rel_GC$Expected_relative_homozygosity-1.96*filename_rel_GC$SD_relative_homozygosity/sqrt(replicate_number)

Plot_recessive <- ggplot()+
 	geom_point(data=filename_rec_GC, aes(x=GC_rate*1000, y=Expected_recessive_load), size=3, shape=15)+
	geom_linerange(data=filename_rec_GC, 
			aes(x=GC_rate*1000, 
			    ymax=GC_rec_max, 
			    ymin=GC_rec_min), size=1)+
	
	labs(y="Homozygous genotypes",
	     x=expression("Mean GC rate per site (" %*% "4 )"),
	      caption=paste("s = 0.005, h = ", h_coefficient, ", mutation rate = 2E-9, population size = 10 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 4 000 bp\nnumber of replicates = 20, sample size = 50, run for 60 000 generations", sep=""))+
	     
	     
	     
	 theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=20, hjust=0.5),
	      axis.title.y=element_text(size=30),
	      axis.title.x=element_text(size=30),
	      
	      axis.text.x=element_text(size=35),
	      axis.text.y=element_text(size=35),
	      
	      legend.position="none")+
	      		      
	  scale_x_log10(breaks = c(10**seq(-8, -2, 1)),
                     labels = trans_format("log10", math_format(10^.x)))
	         
Plot_additive <- ggplot()+
	geom_point(data=filename_add_GC, aes(x=GC_rate*1000, y=Expected_additive_load/1000), size=3, shape=15)+
	geom_linerange(data=filename_add_GC, 
			aes(x=GC_rate*1000, 
			    ymax=GC_add_max/1000, 
			    ymin=GC_add_min/1000), size=1)+
			    
	labs(x=expression("Mean GC rate per site (" %*% "4 )"),
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
	      axis.text.y=element_text(size=35),

	      legend.background=element_rect(colour="black"))+
	
	 scale_x_log10(breaks = c(10**seq(-8, -2, 1)),
                     labels = trans_format("log10", math_format(10^.x)))

Plot_relative <- ggplot()+
 	geom_point(data=filename_rel_GC, aes(x=GC_rate*1000, y=Expected_relative_homozygosity), size=3, shape=15)+
	geom_linerange(data=filename_rel_GC, 
			aes(x=GC_rate*1000, 
			    ymax=GC_rel_max, 
			    ymin=GC_rel_min), size=1)+
	
	labs(y="Relative homozygosity",
	     x=expression("Mean GC rate per site (" %*% "4 )"),
	     title="Accumulation of Mutations\nUnder Asexual Reproduction with GC",
	     caption=paste("s = 0.005, h = ", h_coefficient, ", mutation rate = 2E-9, population size = 10 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 4 000 bp\nnumber of replicates = 20, sample size = 50, run for 60 000 generations", sep=""))+
	     
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
                     labels = trans_format("log10", math_format(10^.x))) +
          scale_y_continuous(limits = c(0,1))

  	
Plot <- Plot_additive/Plot_recessive

plot_name <- paste("Figure_Mutation_accumulation_GC_", h_coefficient, "_vXII_full_60000_newpop.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi = 300,
	width=12,
	height=12)
	
plot_name_2 <- paste("Figure_Mutation_accumulation_GC_relative_homozygosity_", h_coefficient, "_vXII_full_60000_newpop.png",sep="")
ggsave(filename=plot_name_2,
	plot=Plot_relative,
	dpi = 300,
	width=12,
	height=12)
}
