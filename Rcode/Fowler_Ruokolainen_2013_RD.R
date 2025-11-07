# 21/03/24 OIST

# Code to generate Fowler & Ruokolainen (2013, JTB) competition model
#
# Also attempting to maximise the response diversity
# using a mechanistic approach to environmental variation
# along a single resource gradient. Can we generate more 'diversity' in 
# correlation structure with a single stochastic environmental variable
# than the resp-vector approach which leads to perfect ±1 correlation
# values for all values of resp? TL;DR: Yes!

# Extra thoughts for the multistressors discussions with Ceres:
# Do responses to multiple stressors vary from the additive (probably, with interactions and non-linear responses)
# Can/should we treat all stressors as continuous? Are some better as factors (drought, resource quality (Mugabo et al 2019), etc?)
# What are the constraints on a single correlation matrix? We know min(mean(corr(X))) ~ -1/(S-1), but there are likely constraints on positive matrix elements as well. E.g., if 2 species fluctuate in perfect sync, then a 3rd species is constrained in how it can fluctuate with respect to the original 2.
# When are our metrics performing as expected? Use models to test utility.
# 4th Corner: is community assembly related to environment-traits interactions across space. Braga et al (2018)
# What happens when species are no longer at equilibrium (i.e., non-equilibrium dynamics)
# How does adding different interaction types alter things?
# Holling's Discontinuities theory/metric (cross-scale model of resilience) <-  apply to forecasting? How does varying RD (low to high) alter our expectation of resilience, protecting the communities for longer to envtal fluctuations.

rm(list = ls())

TS <- 1000						# Time series length to simulate
S <- 4							# Number of species in the community
mu <- seq(-S/2,S/2, length.out = S)		# vector of each species maximum resource utilisation
# mu <- c(-2.05,-1.95,1.95,2.05)		# Testing with large niche differences between clumped 										# species

sigma_K <- S					# SD of distribution of carrying capacities
								# This should grow with S to avoid overcrowding/infeasible/
								# unstable communities
tau <- scale(rnorm(TS))*sigma_K		# vector of realised environmental values	
									# could also be runif(TS, min(mu), max(mu))

								
K <- exp(-mu^2/(2*sigma_K^2))	# Vector of deterministic carrying capacities
								# used to find the deterministic equilibrium densities
# plot(mu, K)					# to visualise each species niche-position and K(i)-value

sigma_alpha <- (S-1)/S		# SD of each species' resource utlisation kernels. Values ≥ 1 seem 
						# to lead to infeasible equilibria (N*_i < 0 for some i) 
						# (S-1)/S to standardise across S-values?

# If this is randomised/varied for each species (more/less generalist), it could introduce asymmetries into the A-matrix. E.g., is species closer to the environmental mean are expected to be more specialist (because they can afford to as the envt 'favours' them more)
# See the answer(s) here for unequal variances:
# https://stats.stackexchange.com/questions/103800/calculate-probability-area-under-the-overlapping-area-of-two-normal-distributi
sigma_alpha <- matrix(abs(seq(-(S-1)/S, (S-1)/S, length.out = S)), S, S, byrow = TRUE)
						
# Build the Alpha (interaction)-matrix based on the above details, 1's on main diagonal (self-limitation)
A <- as.matrix(exp(-dist(mu, diag = TRUE, upper = TRUE)^2/(4*sigma_alpha^2))) + diag(S)
# A <- diag(S)			# a version with no between-species competition

# Calculate the stochastic series of species-specific Carrying Capacities
Kt <- exp(-(matrix(rep(mu, TS), TS, S, byrow = TRUE) - matrix(rep(tau,S), TS,S, byrow = FALSE))^2/(2*sigma_K^2))

# Nhat_t <- apply(Kt, 2, function(x){solve(A,Kt[x,])})

cor(Kt)									# Correlation matrix across K(i,t)
mean(cor(Kt)[lower.tri(diag(S))])		# mean correlation value
cov(Kt)										# Covariance matrix across K(i,t)
# matplot(Kt, type = "l", lty = 1, lwd = 2)

# Does the chol function work with a realised cor(K) matrix? Not always!
L <- try(chol(cor(Kt)), silent=FALSE)
L%*%tau
# L %*% matrix(rep(tau,S), TS, S, byrow = FALSE)

# Set up the details for simulating the system
r <- 1.75								# Instrinsic growth rate(s - if vector)
N <- matrix(NA, TS, S) -> Nhat_t		# Initiate matrices to store data
N[1,] <- abs(solve(A, K))				# Initial population sizes: equilibrium densities
Nhat <- solve(A, colMeans(Kt))			# Mean of the stochastic attractor
Nhat_t[1,] <- solve(A, Kt[1,])			# Initiate stochastic attractor matrix

# Simulate across time-series of length TS with multi-species (L-V) Ricker dynamics
for (tt in 1:(TS-1)){
	N[tt+1,] <- N[tt,]*exp(r*(1-(N[tt,]%*%A)/Kt[tt,]))		# Realised abundances
	Nhat_t[tt+1,] <- solve(A, Kt[tt,])						# Stochastic attractor
}

# Plot and show some features of the time-series
hist(diff(log(N)))
cor(N)
cor(Nhat_t)
matplot(N, type = "l", lty = 1, lwd = 2, log = "y")
matplot(Nhat_t, type = "l", lty = 1, lwd = 2)
plot(N, Nhat_t, pch = 16, col = matrix(rep(c(1:S),TS), TS,S, byrow = TRUE), log = 'x')
plot(N, Kt, pch = 16, col = matrix(rep(c(1:S),TS), TS,S, byrow = TRUE))