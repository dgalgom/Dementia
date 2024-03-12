# Dementia

This repository serves as data bank for investigating the importance of performance-based physical function in dementia using data from the **National Health and Aging Trends Study** (**NHATS**).

Multi-state modeling was performed using the `msm` package (version 1.6.9), and the estimation of life expectancies were computed using the `elect` package (version 1.2).

Regarding the `NHATS_data` data object, it is the dataset including only forward transitions (as we stated in our research based on the medical evidence of irreversible-and-progressive conditions of dementia), and older adults that completed, at least, two interviews. The variables contained in this dataset are:

  +  `id`: specific identifier for each participant in the NHATS
  +  `state`: clinical diagnosis for dementia. State 1 refers to older adults **free of dementia** (dementia-free state); state 2 refers to older adults with any **mild cognitive impairment** (MCI state; known as the prelude of dementia); state 3 corresponds with a clinical diagnosis of **dementia**; and the state 4 corresponds with participant **death**.
  +  `dep`: dummy variable indicating a depression diagnosis.
  +  `smk_before`: smoking status; whether older adults had been smoked during their lifetimes.
  +  `exercise`: whether older adults were considered physically active on a regular basis.
  +  `income`: stratified income groups from 1 (very-low income) to 5 (high-income).
  +  `sex`: 1 for men, and 2 for women.
  +  `wave`: rounds of NHATS (from 1 in 2011 to 12 in 2023).
  +  `sppb10`: whether or not the person reached 10 or more score in the Short Physical Performance Battery.
  +  `income2`: classification in low-income and high-income; whether the person earned more than 20,000$/year. 
  +  `age`: the centered age of the participants; this variable is centered subtracting the mean sample age (80.55) to the real age.
  +  `sppb`: original score in the Short Physical Performance Battery (from 0 [the worse score] to 12 [the best score]).


   
![Fig_1_dem](https://github.com/dgalgom/Dementia/assets/75797492/ca0d472a-3132-41b3-b3c1-36ac07e14b65)

