This code requires the R library rpsychi.

The Python code will also need rpy2 and SciPy.

For tests with more than 2 samples the Python code implements a greedy (and slow) algorithm to find the range of possible values, while the R code applies an approximate algorithm.  As a result, the ranges for the Python code may be slightly larger than the ranges provided by the R code.