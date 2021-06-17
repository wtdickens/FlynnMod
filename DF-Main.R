#######################################################
#         D&F Development Simulation
#
#     Is There a Young Adult Critical Period
#             
#             Programmer: William Dickens
#                  6/8/21
#
#  This simulation is intended to show what types of
#  models can produce a number of differenet results
#  Among the things this model will be used to look at
#  are whether a D&F model and/or a model with phased
#  in gene expression can explain the changing nature
#  of DZ and MZ correlations as pairs age, whether a
#  D&F model can explain stable IQs in adulthood and 
#  rapidly changning IQs in childhood. An attempt will
#  be made to calibrate the model to actual data
#  but for now I'm only looking for results that are
#  qualitatively consistent with observed data.  
#  
######################################################

library(MASS)  # Import matrix library
library(R6)    # Import library for R6 type class construnction

#
#  Model Parameters
#

N <- 40       # Number of simulated twin pairs
NDZ <- 20     # Number of dizygotic twins
NE <- 1000    # Number of environments
NPI <- 10     # Number of environments per individual
NDim <- 2     # Number of dimensions to environment 
              #  including cognitive demands
NAL <- 200    # Number of binary allels (must be even)
TPPY <- 1     # Time periods per year

#
#   Set up data structures
#

# Parent Genome:
#   Binary matrix with rows = number of allels cols = 2 times number of twin pairs

ParentGenes <- RandomBin(NAL, 2 * N)

#
# Construct Twin Genome
#

#   Matrix of zeros with rows = number of allels, cols = 2 times the number of twin pairs.
#   Will be filled in below. The first half of the columns will be DZ twins and the second
#     half will be MZ twins

TwinGenes <- matrix(0, nrows=NAL, ncols=2*N )

#   ChVec will be used to determine which allels come from parent 1 vs. parent 2

ChVec <- c(matrix(0, 1, NAL/2), matrix(1, 1, NAL/2) )

for (i in 1:N){
  
  # Dizygote twins first -- each gets a random half of each parents genes
  
  # WhichParent is a vector of ones and zeros. When it is zero the twin gets
  #  the corresponding allel from parent 1, and when it is one from parent 2.
  #  The function sample(ChVec) takes a random sample of ChVec without replacement
  #  with length equal to the length of ChVec which is the number of allels 
  WhichParent1 <- sample(ChVec) # Allel selection vector for DZ twin 1
  WhichParent2 <- sample(ChVec) # Allel selection vector for DZ twin 2
  WhichParentMZ <- sample(ChVec)# Allel selection vector for MZ twin pairs
  
  # Loop over allels for all twins
  for (j in 1:NAL) {
    # DZ Twin 1 first
    TwinGenes[j, i * 2 - 1] <- ParentGenes[j, i * 2 - 1 + WhichParent1[j]]
    # DZ Twin 2
    TwinGenes[j, i * 2] <- ParentGenes[j, i * 2 - 1 + WhichParent2[j]]
    # MZ Twin 1
    TwinGenes[j, N + i * 2 - 1] <- ParentGenes[j, N + i * 2 - 1 + WhichParentMZ[j] ] 
  }
  # MZ Twin 2 has same genome as Twin 1 so...
  TwinGenes[, N + i * 2] <- TwinGenes[, N + 1 * 2] 
}
