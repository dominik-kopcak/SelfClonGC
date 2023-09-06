s_parameters <- c(-0.01)
h_parameters <- c(0.2, 0.5)
gamma_parameters <- c(10**(seq(-10, -4, 2)))
lambda_parameters <- c(10**(c(2)))
replicates <- seq(1, 5, 1)

if (file.exists("parameters_all_2.txt")){
	file.remove("parameters_all_2.txt")
}

for (s in s_parameters){

	for (h in h_parameters){
		
		for (gamma in gamma_parameters){
		
			for (lambda in lambda_parameters){
					
				for (replicate in replicates){
			
					write(c(s, h, gamma, lambda, replicate), file = "parameters_all_2.txt", append = TRUE)
					
				}
			}
		}
	}
}
