# Hypothesis-testing-one-sample-and-two-samples-the-QS

Implementing hypothesis testing on the QS data set in R
I used a data set called QS. The QS World University Rankings is an annual publication of global 
university rankings by Quacquarelli Symonds. The Source of my data set is www.kaggle.com. 

in QS_ hypothesis_1 file, I implemented hypothesis testing one sample and two samples found that the mean 
of international students in the top 20 universities is greater than the theoretical mean of 5000 , 
mean of student_faculty ratio in the top 20 universities is less than the theoretical mean of 6 
international students in public universities is not equal to private universities in the top 20 
universities in 2017? 

At first, I checked the normality of the corresponding variables by implementing the Shapiro-Wilk normality test 
and destiny plot. After that, I Specified the Null and Alternative Hypotheses with types of one-samples 
upper and lower one-sided and two samples two-sided for each question. Since the variance and standard 
deviation of the population are unknown, I conducted t-tests. Then I Set up a decision rule and calculated 
the t-test with t.test( ) in R. Finally, I explained the results by including t-test, P-value, and confidence 
interval for accepting or rejecting hypotheses.


in QS_ hypothesis_2, I want to answer the following questions by applying inferential statistics and 
hypothesis testing: 
1- Does student_faculty_ratio effect on the rank of the university? 
2- Does the faculty count effect on the number of international students at private and public 
 universities?
To find the answers, I implemented Shapiro-test and Q-Q plots to find the distribution of my 
variables. Then I applied a correlation matrix to find the correlation between the variables. After that, I 
defined dependent and independent variables for each question and implemented cor. test, scatter plot, 
simple and fixed effects linear regression, and hypothesis t.test to find the answer. Also, I specified the 
Null and Alternative hypotheses and the decision rule for each question. I explained the results by 
including the Coefficient - t value, Coefficient -P-value, The R-squared, t.test P-value, and confidence 
interval for accepting or rejecting hypothese
