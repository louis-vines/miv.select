# R Package Checklist

## Rahul to circulate:

-Data quality checklist
-Single factor analysis Excel Sheet
-Any other useful modelling reading (e.g. that handout you had on the credit modelling procedure at your previous empoyer)
-Package Contents

## Data Quality Checks

 Things to check are consistent through time:

- main statistics: mean / median / IQ range / variance
- na proportion
- anomoly detection
- Does combining features with gb labels disrupt the bad rate? Is the bad rate representitive of now?

## Feature transformation / Single Factor Analysis

- train test split - DONE
- Binning Alg, dual feature binning - DONE
- min / max / transform - DONE
- ranks and plots
- shiny app mimicking Rahul’s Excel sheet?
- missing value imputation
- long list features by gini, iv, p-value
- identify non-linear relationships: sqrt, 1/x, ln, binning, smoothing
- remove correlated features: remove most correlated based on correlation with others, take best IV/gini, varclust take best IV/gini, VIF

## Multifactor analysis / model fitting:

- Marina’s Alg,
- Anuj Alg, Partial IV
- Other stepwise procedures?
- Regularisation

## Model Validation

- Arash’s validation report
- cross validation procedure
- Decile bad rate plot
- gini, ROC curve
- restricted gini (shiny app?)

##additional features:
- deal with missings in the supervised binning alg -> stuff into nearest bad rate or lowest weight of evidence
- above/inside labels in bars
