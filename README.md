# MBTA-On-Time-Prediction-Accuracy-Analysis
This Project is a part of Northeastern University's MPS Analytics Program Class Assignment. 

### **Program Name:** Masters of Professional Studies in Analytics 
### **Course Name:** Intermediate Analytics
### **Course Number:** ALY6015.71629.202515
### **Date :** December 11,2024
### Project Team: Team Epsilon 
### Team Members:
1. Hari Priya Ramamoorthy (ramamoorthy.h@northeastern.edu)
2. Isaac Nyinaku (nyinaku.i@northeastern.edu)
3. Shilin Wang (wang.shili@northeastern.edu)

### Final PPT: [MBTA Prediction Accuracy Analysis Project Insights & Recommendations Presentation.PPT](https://github.com/Haripriya9851/MBTA-On-Time-Prediction-Accuracy-Analysis/blob/main/MBTA_Prediction_Accuracy_ANALYSIS_PPT.pdf)
### Final Report: [MBTA Prediction Accuracy Analysis Project Report.pdf](https://github.com/Haripriya9851/MBTA-On-Time-Prediction-Accuracy-Analysis/blob/main/Report/ALY6015_TeamEpsilon_Final_Analysis_Report_Dec12.pdf)
### Final R Code: [MBTA Prediction Accuracy Analysis Project Rscript.R](https://github.com/Haripriya9851/MBTA-On-Time-Prediction-Accuracy-Analysis/blob/main/R%20code/ALY6015_Epsilon_finalProject_Rscript_Dec12.R)

# Problem Statement :
- The Massachusetts Bay Transportation Authority (MBTA) operates many transport lines (Bus, Red, Green, Orange, & Blue lines) across the Massachusetts state.
- The MBTA wants to improve their On-Time prediction model to provide accurate predictions to improve operational efficiency and ridership.

# Objective of the Analysis: 
- To provide recommendations on significant factors to include in MBTA on-time prediction model for improved accuracy based on statistical tests and Feature selection methods.

# Methodology:
## The analysis was done in several steps leveraging R Programming:
1.	**Exploratory Data Analysis (EDA):** This phase involved handling data integration potential data issues, along with plotting visualization graphs to derive insights and business questions for the relationships between the features and the target variable. 
2.	**Hypothesis Testing:** An ANOVA,Chi-square and Correlation test was conducted to explore the statistical differences in prediction accuracy across transportation modes.
3.	**Linear Modeling:** A linear regression model was fit to predict the accuracy of MBTA’s model, considering route, ridership, and month as predictors.
4.	**Feature Selection:** Both stepwise regression and lasso regression were employed to assess feature importance and select the most relevant predictors.
   
## Statistical Techniques
- **Linear Regression:** This model was used to examine the relationship between the predictors and model accuracy.
- **Stepwise Regression:** This method automatically selected the most significant features based on AIC criteria.
- **Lasso Regression:** Lasso regularization was applied to shrink coefficients of less important features, simplifying the model.



