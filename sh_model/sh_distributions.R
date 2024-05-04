#!/usr/bin/env Rscript
## Distribution of selection coefficients

alpha = 0.2
mean_s = 0.01

beta = alpha/mean_s

total_n_draws_s <- 100000
upper_limit_s <- 0.04

random_s <- rgamma(total_n_draws_s, shape = alpha, rate = beta)
random_s_subset <- random_s[random_s < upper_limit_s]
b <- seq(0, upper_limit_s, 0.001)

not_shown <- total_n_draws_s - length(random_s_subset)
print(not_shown)
print(not_shown/total_n_draws_s*100)

mean(random_s)



## Distribution of dominance coefficients

total_n_draws_h = 100000

k = 91.63
upper_limit_h = 1
bb <- seq(0, upper_limit_h, 0.01)

h_max = exp(-k*random_s)

random_h <- runif(total_n_draws_h, min=0, max=h_max)

random_h_subset <- random_h[random_h < upper_limit_h]


print(mean(random_h)) ## mean of all h, we calibrated only for h whose s is 0.01

not_shown_h <- total_n_draws_h - length(random_h_subset)
print(not_shown_h)
print(not_shown_h/total_n_draws_h*100)

## Joint s-h distribution

joint_s <- rgamma(2000, shape = alpha, rate = beta)
joint_h_max = exp(-k*joint_s)
joint_h <- runif(2000, min=0, max=joint_h_max)

sh_df <- data.frame(s_coeff = joint_s,
                    h_coeff = joint_h)

sh_df_subset <- subset(sh_df, s_coeff < upper_limit_s)

png("sh_joint_distribution.png", width = 15, height = 15, units = "cm", res=600)
plot(sh_df_subset$s_coeff, 
     sh_df_subset$h_coeff, 
     xlab = "Selection coefficient", 
     ylab = "Dominance coefficient", 
     main = "Distribution of fitness effects",
     frame.plot = FALSE)
dev.off()

png("s_marginal_distribution.png", width = 15, height = 15, units = "cm", res=600)
hist_info <- hist(random_s_subset, plot = FALSE)         
hist_info$density <- hist_info$counts/sum(hist_info$counts) * 100
plot(hist_info, freq = FALSE)   
dev.off()

png("h_marginal_distribution.png", width = 15, height = 15, units = "cm", res=600)
hist_info <- hist(random_h_subset, plot = FALSE)         
hist_info$density <- hist_info$counts/sum(hist_info$counts) * 100
plot(hist_info, freq = FALSE)  
dev.off()
