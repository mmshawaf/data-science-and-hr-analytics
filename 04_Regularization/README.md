Made an R file with Professor Kapoor’s code

Changed number of folds to 20, made the program take longer, learned that k-fold cross-validation is by no means a fast program, could be an issue for big data

This changed the result of generalized R2 to -2.55, smaller but still negative
Learned that the minimum value can be smaller than -1
This made me think about the maximum value it can take on, 
So I ChatGPT about it and learned that it shouldn't be larger than 1 unless “the metric is not strictly bounded or normalized”

Running the code for forward stepwise regression, it took a very long time and the console got flooded, making the history inaccessible, it would be ideal for it not to show every single update. 
I got the same length as in the lecture slides, which makes sense as FSR follows a strict algorithm and shouldn’t change from run to run

Running the Lasso Regularization Path was significantly faster, even with the fact that the browser_domains.csv file is five times as large as the one used for FSR, I would definitely take this into account if I need to regularize some data in the future.

After running the code for KFCVFL multiple times inspected the coefficient for vistaprint.com and the coefficient was the same each time, this was not what I was expecting as the folds should be drawn randomly. 
