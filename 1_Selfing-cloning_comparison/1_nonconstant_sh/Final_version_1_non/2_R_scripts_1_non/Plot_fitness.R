library(dplyr)
library(ggplot2)

Uniparenting_rates <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.95, 0.99, 1.00)

Max_time=60000

N_replicates = 20

Fitness_selfing <- read.csv("Data_fitness_evolution_selfing.csv")
Fitness_selfing$Uniparenting_rate <- as.factor(Fitness_selfing$Uniparenting_rate)

Fitness_cloning <- read.csv("Data_fitness_evolution_cloning.csv")
Fitness_cloning$Uniparenting_rate <- as.factor(Fitness_cloning$Uniparenting_rate)



Plot_mean_selfing <- 
	ggplot(data=Fitness_selfing, aes(x=Time, y=Expected_mean_fitness), size=1) +
	geom_line(data=Fitness_selfing, aes(x=Time, y=Expected_mean_fitness, colour = Uniparenting_rate))+
	geom_ribbon(data=Fitness_selfing, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates), ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates), fill = Uniparenting_rate), alpha = 0.5)
	
Plot_mean_cloning <-
	ggplot(data=Fitness_cloning, aes(x=Time, y=Expected_mean_fitness), size=1) +
	geom_line(data=Fitness_cloning, aes(x=Time, y=Expected_mean_fitness, colour = Uniparenting_rate))+
	geom_ribbon(data=Fitness_cloning, aes(x=Time, ymin=Expected_mean_fitness - 1.96*SD_mean_fitness/sqrt(N_replicates), ymax=Expected_mean_fitness + 1.96*SD_mean_fitness/sqrt(N_replicates), fill = Uniparenting_rate), alpha = 0.5)
		
Plot_variance_selfing <-
	ggplot(data=Fitness_selfing, aes(x=Time, y=Expected_variance_fitness), size=1) +
	geom_line(data=Fitness_selfing, aes(x=Time, y=Expected_variance_fitness, colour = Uniparenting_rate))+
	geom_ribbon(data=Fitness_selfing, aes(x=Time, ymin=(Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates)), ymax=(Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates)), fill = Uniparenting_rate), alpha = 0.5)
	
Plot_variance_cloning <- 
	ggplot(data=Fitness_cloning, aes(x=Time, y=Expected_variance_fitness), size=1) +
	geom_line(data=Fitness_cloning, aes(x=Time, y=Expected_variance_fitness, colour = Uniparenting_rate))+
	geom_ribbon(data=Fitness_cloning, aes(x=Time, ymin=Expected_variance_fitness - 1.96*SD_variance_fitness/sqrt(N_replicates), ymax=Expected_variance_fitness + 1.96*SD_variance_fitness/sqrt(N_replicates), fill = Uniparenting_rate), alpha = 0.5)



Plot_mean_selfing <- Plot_mean_selfing +


scale_fill_viridis_d() + 
scale_colour_viridis_d(guide = "none") + 
labs(title="Population Fitness\nUnder Selfing Reproduction",
     x="Time",
     y="Mean fitness",
     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 20, sample size = 50, run for 60 000 generations", sep=""))+
guides(colour = guide_legend(title = "Selfing rate", override.aes = list(size = 10)),
       fill = "none") +

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
   
   legend.key = element_blank(),
   legend.key.size = unit(1, "cm"))
   
Plot_mean_cloning <- Plot_mean_cloning +
scale_fill_viridis_d() +
scale_colour_viridis_d(guide = "none") + 
labs(title="Population Fitness\nunder Asexual Reproduction",
     x="Time",
     y="Mean fitness",
     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 20, sample size = 50, run for 60 000 generations", sep=""))+
guides(colour = guide_legend(title = "Cloning rate", override.aes = list(size = 10)),
       fill = "none") +

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
   
   legend.key = element_blank(),
   legend.key.size = unit(1, "cm"))
   
Plot_variance_selfing <- Plot_variance_selfing +
scale_colour_viridis_d(guide = "none") + 
scale_fill_viridis_d() + 
labs(title="Population Fitness Variance\nUnder Selfing Reproduction",
     x="Time",
     y="Fitness variance",
     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 20, sample size = 50, run for 60 000 generations", sep=""))+
guides(colour = guide_legend(title = "Selfing rate", override.aes = list(size = 10)),
       fill = "none") +

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
   
   legend.key = element_blank(),
   legend.key.size = unit(1, "cm"))

Plot_variance_cloning <- Plot_variance_cloning +
scale_colour_viridis_d(guide = "none") + 
scale_fill_viridis_d() +  
labs(title="Population Fitness Variance\nUnder Asexual Reproduction",
     x="Time",
     y="Fitness variance",
     caption=paste("sh model, mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 20, sample size = 50, run for 60 000 generations", sep=""))+
guides(colour = guide_legend(title = "Cloning rate", override.aes = list(size = 10)),
       fill = "none") +

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
   
   legend.key = element_blank(),
   legend.key.size = unit(1, "cm"))


plot_mean_selfing_name <- paste("Figure_mean_fitness_selfing_nonconstant_sh.png", sep="")
ggsave(filename = plot_mean_selfing_name,
plot = Plot_mean_selfing,
dpi = 600)

plot_mean_cloning_name <- paste("Figure_mean_fitness_cloning_nonconstant_sh.png", sep="")
ggsave(filename = plot_mean_cloning_name,
plot = Plot_mean_cloning,
dpi = 600)

plot_variance_selfing_name <- paste("Figure_variance_fitness_selfing_nonconstant_sh.png", sep="")
ggsave(filename = plot_variance_selfing_name,
plot = Plot_variance_selfing,
dpi = 600)

plot_variance_cloning_name <- paste("Figure_variance_fitness_cloning_nonconstant_sh.png", sep="")
ggsave(filename = plot_variance_cloning_name,
plot = Plot_variance_cloning,
dpi = 600)

