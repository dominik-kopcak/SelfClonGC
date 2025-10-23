library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)
library(magrittr)

replicate_number=100

# Functions for calculating expected mutation counts
pa_asex <- function(mu,g,s,h){
	return (((s + g - s*g)*mu)/(s*(g + h*s*(1 - g))))
}

F_asex <- function(g,s){
	return (g/(s + g - s*g))
}

N_muts <- function(Na,L,mu,g,s,h){
	2*Na*L*pa_asex(mu,g,s,h)
}

N_homs <- function(Na,L,mu,g,s,h){
	2*Na*L*(pa_asex(mu,g,s,h)^2 + pa_asex(mu,g,s,h)*(1-pa_asex(mu,g,s,h))*F_asex(g,s))
}

# predictions
gamma_values <- c(10**seq(-11, -5, 1))*4000
nmut <- N_muts(5000,25e6,4e-9,gamma_values,0.01,0.2)
nhoms <- N_homs(5000,25e6,4e-9,gamma_values,0.01,0.2)

GC_recessive_load <- read.csv("Data_GC_recessive_load_vXII_full_60000.csv")
GC_additive_load <- read.csv("Data_GC_additive_load_vXII_full_60000.csv")
GC_relative_homozygosity <- read.csv("Data_GC_relative_homozygosity_vXII_full_60000.csv")

h_coefficient <- 0.2

filename_rec_GC <- GC_recessive_load %>% filter(Dominance_coefficient == h_coefficient)
filename_add_GC <- GC_additive_load %>% filter(Dominance_coefficient == h_coefficient)
filename_rel_GC <- GC_relative_homozygosity %>% filter(Dominance_coefficient == h_coefficient)
#filename_fix_GC <- GC_fixed_mutations %>% filter(Dominance_coefficient == h_coefficient)

GC_rec_max <- filename_rec_GC$Expected_recessive_load+1.96*filename_rec_GC$SD_recessive_load/sqrt(replicate_number)
GC_rec_min <- filename_rec_GC$Expected_recessive_load-1.96*filename_rec_GC$SD_recessive_load/sqrt(replicate_number)

GC_add_max <- filename_add_GC$Expected_additive_load+1.96*filename_add_GC$SD_additive_load/sqrt(replicate_number)
GC_add_min <- filename_add_GC$Expected_additive_load-1.96*filename_add_GC$SD_additive_load/sqrt(replicate_number)

GC_rel_max <- filename_rel_GC$Expected_relative_homozygosity+1.96*filename_rel_GC$SD_relative_homozygosity/sqrt(replicate_number)
GC_rel_min <- filename_rel_GC$Expected_relative_homozygosity-1.96*filename_rel_GC$SD_relative_homozygosity/sqrt(replicate_number)

# adding in analytical results
# First for recessive load
filename_rec_GC %<>% mutate(Type="Simulation")
filename_rec_GC2 <- filename_rec_GC
filename_rec_GC2 %<>% mutate(X = c(8:14),SD_recessive_load=0,Type="Analytics",Expected_recessive_load=nhoms)
GC_rec_max <- c(GC_rec_max,filename_rec_GC2$Expected_recessive_load)
GC_rec_min <- c(GC_rec_min,filename_rec_GC2$Expected_recessive_load)
filename_rec_GC <- rbind(filename_rec_GC,filename_rec_GC2)

# Next for additive load
filename_add_GC %<>% mutate(Type="Simulation")
filename_add_GC2 <- filename_add_GC
filename_add_GC2 %<>% mutate(X = c(8:14),SD_additive_load=0,Type="Analytics",Expected_additive_load=nmut)
GC_add_max <- c(GC_add_max,filename_add_GC2$Expected_additive_load)
GC_add_min <- c(GC_add_min,filename_add_GC2$Expected_additive_load)
filename_add_GC <- rbind(filename_add_GC, filename_add_GC2)

Plot_recessive <- ggplot()+
 	geom_point(data=filename_rec_GC, aes(x=GC_rate*1000, y=Expected_recessive_load, shape=factor(Type,levels=c('Simulation','Analytics'))), size=3)+
 	scale_shape_manual(name="Type",values=c(19, 2))+
	geom_linerange(data=filename_rec_GC, 
			aes(x=GC_rate*1000, 
			    ymax=GC_rec_max, 
			    ymin=GC_rec_min), linewidth=1)+
	
	labs(y="Homozygous genotypes",
	     x=expression("Mean GC rate per site (" %*% "4 )"),
	      caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, mean GC tract length = 4 000 bp\nnumber of replicates = 100, sample size = 50, run for 60 000 generations", sep=""))+
	     
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
                     labels = trans_format("log10", math_format(10^.x)))+
	  scale_y_log10(breaks = c(10**seq(1, 5, 1)),
                     labels = c('10','100','1000','10,000','100,000'))                     
	         
Plot_additive <- ggplot()+
	geom_point(data=filename_add_GC, aes(x=GC_rate*1000, y=Expected_additive_load/1000, shape=factor(Type,levels=c('Simulation','Analytics'))), size=3)+
	scale_shape_manual(name="Type",values=c(19, 2))+
	geom_linerange(data=filename_add_GC, 
			aes(x=GC_rate*1000, 
			    ymax=GC_add_max/1000, 
			    ymin=GC_add_min/1000), linewidth=1)+
			    
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
		  legend.title = element_text(size=20),
	      legend.text = element_text(size=20))+
	
	 scale_x_log10(breaks = c(10**seq(-8, -2, 1)),
                     labels = trans_format("log10", math_format(10^.x)))

  	
Plot <- Plot_additive/Plot_recessive

plot_name <- paste("Figure_Mutation_accumulation_GC_", h_coefficient, "_vXII_full_60000_withpreds.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi = 300,
	width=12,
	height=12)
