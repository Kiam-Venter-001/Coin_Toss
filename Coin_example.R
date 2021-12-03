#packages
library(markovchain)
require('HMM')

###################
#HMM for coin toss#
###################
#state 1 in hidden is unbiased coin
#state 2 is biased coin
###################
#hidden state transition probabilities
a11 = 0.8
a12 = 0.2
a21 = 0.3
a22 = 0.7

#hidden state transition matrix
A = matrix(c(a11,a12,a21,a22), nrow = 2, ncol = 2, byrow = TRUE)

#observable state transition probabilities
b1H = 0.5
b1T = 0.5
b2H = 0.6
b2T = 0.4

#sovservable state vectors
b1 = c(b1H,b1T)
b2 = c(b2H,b2T)

#matrix of hidden to observable
B = matrix(c(b1,b2), nrow = 2, ncol = 2, byrow = TRUE)

#markov chain
mc = new("markovchain", transitionMatrix = A, states = c("Unbiased", "Biased"))

#creating test data
n = 100
Data = markovchainSequence(n, mc)

test_dat = function(d, b)
{
  if(d == "Unbiased")
  {
    temp_bool = sample(c(0,1), 1, replace = T, prob = b[1,])
    if(temp_bool == 0)
    {
      t = "H"
    }else
    {
      t = "T"
    }
  }else
  {
    temp_bool = sample(c(0,1), 1, replace = T, prob = b[2,])
    if(temp_bool == 0)
    {
      t = "H"
    }else
    {
      t = "T"
    }
  }
  return(t)
}

set.seed(120)
Test_data = numeric(n)
for(i in 1:n)
{
  Test_data[i] = test_dat(Data[i], B)
}

#hmm creation
hmm_cointoss = initHMM(c("Unbiased", "Biased"), c("H", "T"), transProbs = A, emissionProbs = B)

#test using baumwelch
bw = baumWelch(hmm_cointoss, Test_data, maxIterations=100, delta=1E-9, pseudoCount=0)

#actual vs estimated
actual_hidden = hmm_cointoss$transProbs
estimated_hidden = bw$hmm$transProbs

actual_observed = hmm_cointoss$emissionProbs
estimated_observed = bw$hmm$emissionProbs

actual_hidden
actual_observed

estimated_hidden
estimated_observed

States_visited = Test_data
Observation_sequence = Data




