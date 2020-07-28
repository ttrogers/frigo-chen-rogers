# frigo-chen-rogers
 False beliefs study: Data, analysis, and models
 
 ## Description of repository
 This repository contains de-identified data, statistical analysis, and models supporting the work described in the following manuscript:
 
 Frigo, V., Chen, L. and Rogers, T. T. (submitted). A cognitive mechanism for the persistence of widespread false beliefs.
 
 ## Abstract
 
 Cognitive science commonly assumes human learners combine prior knowledge with new information to form statistically optimal beliefs. Why then are false beliefs so resilient and widespread? We develop and test a new hypothesis stemming from computational models of human category learning. In four experiments we found that learners weight conflicting information sources pursuant to a nonlinear function dependent on the learners’ initial beliefs. Highly agreeable sources receive strong weight preventing belief change, but this deference drops sharply and then flattens with increasing disparity, allowing even very distal sources to effect belief change. To understand the implications of this bias for the maintenance of false beliefs in society, we simulated groups of agents learning from one another and from a truth-telling oracle. The learning bias often led agents to fractionate into mutually distrustful groups or to converge on a common false belief. Thus, opposing belief bubbles in contemporary society may reflect stable states in a dynamic system composed of individual learners communicating their beliefs to one another and updating those beliefs according to an evidence-weighting function that strongly favors close agreement but, in the absence of such agreement, never completely discounts extreme voices.

## Structure of repository

Each subdirectory contains files sufficient to replicate all results reported in the manuscript, including analysis of empirical data and simulation results. Each directory contains a JuPyteR notebook file that documents the analysis and contains all code needed to run the analyses interactively, using an R kernel. For Experiment subdirectories, de-identified behavioral data are contained in .csv files. 
