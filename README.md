# Project_11
Capstone Project on building application scorecard for a bank, using customer and credit bureau data-sets.

**Business Understanding**

CredX is a leading credit card provider that gets thousands of credit card applicants every year. But in the past few years, it has experienced an increase in credit loss. The CEO believes that the best strategy to mitigate credit risk is to 'acquire the right customers'.

In this project, you will help CredX identify the right customers using predictive models. Using past data of the bank's applicants, you need to determine the factors affecting credit risk, create strategies to mitigate the acquisition risk and assess the financial benefit of your project.

**Understanding the data**

There are two data sets in this project - demographic and credit bureau data.

Demographic/application data: This is obtained from the information provided by the applicants at the time of credit card application. It contains customer-level information on age, gender, income, marital status, etc.

Credit bureau: This is taken from the credit bureau and contains variables such as 'number of times 30 DPD or worse in last 3/6/12 months', 'outstanding balance', 'number of trades', etc.

Both files contain a performance tag which represents whether the applicant has gone 90 days past due or worse in the past 12-months (i.e. defaulted) after getting a credit card.

In some cases, you will find that all the variables in the credit bureau data are zero and credit card utilisation is missing. These represent cases in which there is a no-hit in the credit bureau. You will also find cases with credit card utilisation missing. These are the cases in which the applicant does not have any other credit card.

**Scope of work**

* Data Cleaning & Preparation

Firstly combine all the data files to get all the required variables.
Basic EDA - Univariate analysis, Bivariate analysis & Segmented analysis
Use concepts like WOE and IV where ever necessary.
Create a file that stores only the WOE values of the variables

* Model Building - Two Models needs to be created

Demographic data model: Build a model to predict the likelihood of default using only the demographic data. This will give you a good idea of the predictive power of the application data. Obviously, the final model will use the credit bureau data as well, though this model is an important part of understanding the predictive power of application data.

Model using both demographic and credit bureau data: Build a model to predict default using both the data sets. You may choose any type of model, though it is recommended to start with a logistic regression model first. Further, you can choose any type of model.

* Model Evaluation

Evaluate the models using relevant metrics and report the results.

* Model Deployment - Building an Application ScoreCard

Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points.

