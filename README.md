README


## Data Curation and Cleaning

The original data pulls and transformations are described in ```./code/create_full_data_from_SPARK_csvs.R```
The current, most-up-to-date original items are ./data/reverse_score_corrected_data_2_26_25.rds
The original data is not available via this repository, but is available to qualified researchers through [SFARI Base](https://base.sfari.org/)

## Agreement and Construct*Item Selection

Delphi votes for construct validity are pulled in ```./code/construct_voting/agreement_matrix.R``` Cohen's and Fleiss's Kappa are also calculated in this script.
Using utilities to automatically write lavaan syntax, ```./code/CFAs/CFA_syntax.rds``` is created here as well, and utilized in creating the measurement models.

## Measurement Models

CFAs were all run from ```./code/CFAs/CFA_All_Constructs_1_30_25.R```, calculating factor score estimates and scaling the data, which are stored in:
```.data/factor_score_estimates_constructs_symbolism_2_10_25.rds```
Additionally, ```items_list_constructs.rds``` is created here, and used throughout the rest of the process to reference which items were selected for each construct.
A short note: the factor score estimate date is before the reverse scored data (2_26_25), the reverse scored data are identical to the data used here, with everything flipped in sign. 
We were able to replicate the factor_score_estimates in ```./data/factor_score_estimates_constructs_symbolism_7_30_25_replication.rds```. 

The items for each construct/CFA are stored in ```./misc/items_list_constructs.rds```


## Structural Model Evaluations

The lavaan versions of the models, as well as the SEM Plots are from: 
```./code/Results_Analyses/MostofskyEwen_Testing.R```
```./code/Results_Analyses/RogersPennington_Testing.R```
```./code/Results_Analyses/OrnitzRitvo_Testing.R```

and are stored in ```./code/Results_Analyses/lavaan_models/```

## Null Distributions

To compare to a null distribution, Monte Carlo simulations were run from random related items (using the same test), excluding the real items.

The most up-to-date sampling function is ```sim_betas_lavaan_std_all()``` 
Note: ```sim_betas_lavaan()``` was deprecated on Aug-11-2025 for using $est rather than $std.all

Because this is a novel procedure, sensitivity analyses were run, completing multiple simulations, varying a few parameters. The parameters were:

1. Reliability of the pseudo-construct must be within .15 of the real construct
2. Naturally overlapping items between constructs - this was specified such that item sampling was done less randomly, and prescribed that, for example, Self.Nonself and RC.F1 had exactly one overlapping item. This opens a can of worms, wherein you must decide how to handle constructs with no overlapping items. Should you allow the same item to freely end up on a pseudo-construct it is compared to?
3. Whether two pseudoconstructs can just "swap items" - you don't want real RC items on pseudo-imitation and real imitation items on pseudo-RC.

Each theory should have three versions of the nulls: one in which all constraints were used, one in which only reliability was constrained, and one in which no constraints were used. These end in ```_all_constraints.R```, ```_reliability_constraints.R``` and ```_no_constraints.R```. 

These three versions are stored in their respective folders inside of ```./code/Empirical_priors/```

p-value calculation was done via permutation testing in ```./code/Empirical_priors/permutation_theory_testing.R```


