Made an R file with Professor Kapoor’s code

Tested the KNN algorithm with k = 1-5, a lot of differences between each one, although some of the observations are classified the same through all 5 k’s.

As we change the classification rule from 1/5 to 1/10, the false positive rate increases to 0.68 from 0.60, as we put more weights on defaults. The false negative rate decreases to 0.04 from 0.08. Conversely, changing the classification rule to 1/2 FPR drops to 0.32 and the FNR rises to 0.25, there is a balance to be had between calling too many defaults incorrectly and letting too many defaults go unnoticed. 

At rule = 1/5, sensitivity = 0.92, specificity = 0.39
At rule = 1/10, sensitivity = 0.99, specificity = 0.08
This is due to the drastic number of changes in true positives and negatives between the two rules.
