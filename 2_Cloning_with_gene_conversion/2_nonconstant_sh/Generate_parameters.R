gamma_parameters <- c(10**(seq(-7, -5, 1)))
lambda_parameters <- c(10**(c(2)))
replicates <- seq(1, 5, 1)

if (file.exists("parameters_lambda_100_nonC_sh.txt")){
	file.remove("parameters_lambda_100_nonC_sh.txt")
}
		
for (gamma in gamma_parameters){

	for (lambda in lambda_parameters){
			
		for (replicate in replicates){
	
			write(c(gamma, lambda, replicate), file = "parameters_lambda_100_nonC_sh.txt", append = TRUE)
			
		}
	}
}
