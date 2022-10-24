#' # APPLIED BAYESIAN TIME SERIES: ECON 7300
#' ## PROBLEM SET I: MCMC methods
#' Instructions:Write up answers and submit in pdf format. Submit code separately and donot mix code into your answers. You can work in pairs. If you choose to do so, name both stu-dents on front page of document

Let the mixture-normal density be your target density
const 𝜇₁ = 0
const σ₁ = 1
const 𝜇₂ = 1
const σ₂ = 1/2
λ = 1/2
N(x,𝜇,σ) = (σ*sqrt(2*π))^-1 * exp(-1/2*((x-𝜇)/σ)^2 )
p(θ) = λ*N(θ,𝜇₁,σ₁)+ (1-λ)*N(θ,𝜇₂,σ₂)
q(θ) = exp(N(λ*𝜇₁ + (1-λ)*𝜇₂,λ*σ₁ + (1-λ)*σ₂))
c = .32

ReqDraws = []
for i in 1:10
	V = Float64[]
	stop = false
	while !stop
		accept = false
		while !accept
			θ̂ = exp((λ*σ₁+(1-λ)*σ₂)*randn()- λ*𝜇₁ + (1-λ)*𝜇₂) 
			U = rand()
			accept = ( U <= p(θ̂)/(c*q(θ̂)) )
			if accept push!(V,θ̂) end
		end
		stopMean = (abs(mean(V)-(λ*𝜇₁+(1-λ)*𝜇₂)) <= 0.01 )
		stopVar = (abs(var(V)-(λ^2*σ₁^2 + (1-λ)^2*σ₂^2)) <= 0.01 )
		println("Diff in Var: $(var(V)-(λ^2*σ₁^2 + (1-λ)^2*σ₂^2)), diff in mean: $(mean(V)-(λ*𝜇₁+(1-λ)*𝜇₂)) ")
		stop = (stopMean & stopVar)
	end
	println(i)
	push!(ReqDraws, length(V))
end


#' Use the accept-reject method to sample from the target density
#' a) What is your source density and why did you choose it?
#'	My source density is Normal(1,1.5). I chose it because we have a mixture of independent normals, so Normal(μ₁+μ₂,σ₁+σ₂) seemed like a good guess for something close to the shape of the target density that would "majorize" it (for some c). Additionally, my source density will attain its maximum at the same place as the target. I plotted a figure to check this visually. I assert that a normal is easy to sample from because Julia has randn built-in.
#' b) Plot the simulated density.
#' c) What is the value of the constant c and how did you choose it?
#' ``c:= sup_{θ ∈ Θ} \frac{p(θ)}{q(θ)}``. I had an intial guess of c=2, which turns out to be pretty close. The picture above suggests that c is attained at θ = 1, which gives c = p(1)/q(1) ≈ 1.9548979947844751
#' d) How many draws do you need on average over 10 runs to get a simulated meanμθ≡E(θ) and varianceσ2θ≡E(θ−μθ)2within 0.01 of their true values?
#' e)  If you use the target density as the source density, how many draws do you need?