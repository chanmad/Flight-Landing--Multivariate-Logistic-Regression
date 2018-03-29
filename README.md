# Flight-Landing--Multivariate-Logistic-Regression
Various data modeling techniques applied on simulated Flight landing data

Final model:

Output	(Intercept)	aircraftboeing	speed_ground	height
1000<LD<2500	-22.99211	4.044287	0.2414531	0.1527719
LD>2500	-120.74928	8.964335	1.1215225	0.368731

•	Final model provides the values of the coefficients and their standard errors, along with the overall deviance and AIC for the model. The model was finalized through stepwise variable selection process based on AIC and chi sq test.
•	The model can be interpreted as defining log odds against the baseline class of Y=1 (LD<1000) for each of the classes Y=2(LD is in the range of 1000 to 2500) and Y=3 (LD>2500).
•	A one-unit increase in the height can affect an increase in the log odds of having a landing>2500 vs. landing distance <1000 by 0.37    
•	The model has a very low misclassification as seen below:
Prediction	Actual
 	1	2	3
1	198	28	0
2	34	381	6
3	0	4	90


