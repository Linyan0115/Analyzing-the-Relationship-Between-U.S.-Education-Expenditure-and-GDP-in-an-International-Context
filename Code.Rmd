---
title: "STAT 240 Project"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE,
                      error = TRUE, fig.height = 4)
library(tidyverse)
```

## Author

**Linyan Li**

### Raw Dataset Reading and Cleaning

```{r}
dataset = read_csv("~/Desktop/STAT 240/data/USA_educationBudget_GDP.csv")
```

```{r}
# Cleaning the dataset
dataset <- dataset %>% 
  drop_na(GDP, BUDGET_ON_EDUCATION, RATIO)

head(dataset,3)
```

### Introduction
As society progresses, education has become a critical area of focus. A common assumption is that a country's education budget is closely related to its GDP. But is this really true? According to the <a id="cite-1"></a> OECD (2021)[1](#ref-1), the average ratio of education to GDP in member countries is 4.9%. As a member of this organisation, does the U.S. exceed this average? **This project will first explore the relationship between education and GDP in U.S., then investigate whether the average U.S. education-to-GDP ratio exceeds the average ratio for OECD countries.** In this project, I have discovered a positive linear relationship between GDP and the education budget for a given year. It is confident that the average GDP to education budget ratio of the United States exceeds the average ratio of OECD member countries. My findings can be used by others as foundations to ask deeper questions like do wealthier countries tend to invest heavier in education? And does a strong education system in many nations depend on their GDP? As college students who are not all American this is of great interest to us to see how the US compares within itself and how it compares to other countries with their Education to GDP ratio.

### Background
To explore my question, I use a dataset that records the annual GDP, education budgets and their ratio in the U.S., as described below:

#### Dataset (USA_educationBudget_GDP.csv)

<a id="cite-2"></a>The dataset[2](#ref-2) I'm working with provides detailed information on the annual federal education budget, GDP values, and the ratio of the two for the United States. It covers the years 1976 to 2016, recording three key columns: `YEAR`, `BUDGET_ON_EDUCATION`, and `RATIO`. It is important to note that GDP values and Budget numbers are in millions of US Dollars.
 
The `YEAR` column indicates the specific year of the recorded data, which helps us track the data over time. The `BUDGET_ON_EDUCATION` column represents the annual federal education budget for that particular year in millions of USD, providing valuable insights into trends in education funding over the years. The `RATIO` column, which is the proportion of the education budget to GDP, is particularly useful for analyzing the relative investment in education over time, offering a deeper understanding of how education funding compares to the overall economic output of the country.
 
It is important to note that no columns were renamed in the dataset, as the original column names align directly with my analytical objectives and are perfectly suited for the analysis.


#### Initial Impressions

```{r}
# Figure1: GDP of the United States per Year
ggplot(dataset, mapping = aes(x=YEAR, y=GDP)) + 
  geom_col(fill = "skyblue", color = "black") + 
  labs(title = "GDP of the United States per Year (1976–2016)", 
       y = "GDP of the United States", 
       x = "Year") + 
  theme_bw() +  
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " Trillion"))

# Figure2: United States Education Budget Per Year
ggplot(dataset, mapping = aes(x=YEAR, y=BUDGET_ON_EDUCATION)) +
  geom_col(fill = "midnightblue", color = "black") + 
  labs(title  = "United States Education Budget Per Year (1976–2016)", 
       x = "Year", 
       y = "US Education Budget") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = " Billion"))

# Figure3: Education Budget and GDP of the United States
ggplot(dataset, mapping = aes(x=GDP, y=BUDGET_ON_EDUCATION)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(se = FALSE, method = "lm") + 
  labs(title = "Education Budget and GDP of the United States (1976–2016)", 
       x = "GDP of the US", 
       y = "US Education Budget") + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = " Billion")) + 
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = " Trillion"))

# Figure4: Distribution of Education Budget-to-GDP Ratio in the U.S.
ggplot (dataset, aes (x = RATIO) ) +
  geom_density(fill = "pink", alpha = 0.5) +
  geom_rug(sides = "b") +
  labs(title = "Distribution of Education Budget-to-GDP Ratio in the U.S. (1976–2016)", x = "Ratio", y = "Approximate Count")

```

From the histogram, it can be seen that as the year changes, the GDP increases. I can also see that the Education Budget fluctuates from year to year. 

From the scatter plot, GDP and education budget seem to have a linear relationship that shows that as GDP increases, so does the Budget for Education. This is true for most of the data, however there are some discrepancies as the GDP gets bigger.

The last plot is a KDE (Kernel Density Estimation) plot with ticks. KDE is a method that creates a smooth curve to estimate the data's distribution. The ticks at the bottom indicate the exact positions of individual data points, providing additional context to the density curve. It shows that the Education Budget-GDP ratio is mostly concentrated around a central value, meaning most data points fall within a specific range. However, the tail extending toward higher ratios indicates some variability in how education budgets are allocated compared to GDP.

Next, I will first calculate the correlation coefficient to assess whether there is a linear relationship between the two variables. Then, I will conduct a hypothesis test to determine whether the average U.S. education-to-GDP ratio exceeds 4.9%.

### Statistical Analysis

#### Calculate the Correlation Coefficient (r)

-   As learned in this class, the correlation coefficient (r) measure of the strength of a linear relationship between two continuous variables. So I explore the relationship between GDP and education budget by calculating their correlation coefficients.

-   Formula:
$$
r = 
\mathsf{Corr}(x,y) = \frac{1}{n-1}\sum_{i=1}^n
\left(\frac{x_i - \bar{x}}{s_x} \right)
\left(\frac{y_i - \bar{y}}{s_y} \right)
$$

-   I use R code to calculate the correlation coefficient between GDP and Education budget:

```{r}
correlation_coefficient <- cor(dataset$GDP, dataset$BUDGET_ON_EDUCATION)
 
print(correlation_coefficient)
```

-   **Interpretation**: Due to \( r = 0.821 \), I can conclude that the U.S. education budget has a strong positive linear relationship with GDP. This suggests that as the GDP increases, the education budget tends to increase as well, with a relatively strong association between the two variables.

#### Statistical Model and Hypotheses

By computing r, I address the question of whether there is a linear relationship between GDP and the education budget. To address whether the average U.S. education budget as a percentage of GDP exceeds the average OECD ratio (4.9%), I test this with an T-test.

##### Statistical Model

Let $x_i$ represent the ratio of the education budget to GDP RATIO (`RATIO`) in the $i$-th year of the dataset. These random variables are modeled by

$$
X_i∼D(\mu,\sigma) \qquad i=1,2,3,…,n
$$

Where $X_i$ represents the $i$-th year's education budget-to-GDP ratio (`RATIO`). $\mu$ is the true underlying average ratio for the population, and $\sigma$ is the true underlying standard deviation of the ratios.

The only assumption with this model is that the ratios are independent across years — the ratio for one year does not affect the ratio for any other year.

While one could argue that historical trends in economic conditions or educational priorities might introduce some dependencies, I will assume independence for the purposes of this analysis.


##### Hypotheses

With this model, I will conduct hypothesis testing with

$$
H_0: μ≤0.049 \\ H_a:μ>0.049
$$ 
using a one-tail t-test to determine if the mean ratio exceeds 0.049.

#### Interpretation

```{r}
xbar <- mean(dataset$RATIO)
s <- sd(dataset$RATIO)
n <- length(dataset$RATIO)

test_stat = (xbar - 0.049) / (s / sqrt(n))
p_value <- pt(abs(test_stat), df = n - 1, lower.tail = FALSE)

p_value
```

> Due to P-value equals to $3.195643×10^{−24}$ (close to zero) , I have enough confidence to reject the null hypothesis. This indicates that the mean education budget-to-GDP ratio is significantly greater than 0.049. Therefore, the U.S. education-to-GDP ratio exceeds the average ratio for OECD countries.

### Discussion

#### Further Interpretation
As analyzed above, I believe that the U.S. education budget and GDP are positively related and that the U.S. average education budget to GDP ratio is higher than the average ratio of OECD member countries, which indicates that economic growth can promote education development and that the U.S. has invested a relatively large amount of economic resources in the field of education. The U.S. should continue to maintain a high percentage of its education budget and optimize the efficiency of its use of funds by striving to increase fiscal revenues so that the government can allocate more resources to education.

#### Short-comings and Future Work

-   **Short-comings**
    1. My dataset only goes until 2016 and therefore is a little outdated by eight years-worth of data from 2016 to now, including present data not being contained in o current dataset.
    
    2. My Education Budget data is very general. There can be questions as to what within the education field gets funding within schools. School athletics and construction on a new school parking lot could still be for education purposes but can be misleading for some people because the money is not directly reinforcing the classroom studies.
    
    3. A shortcoming that I think I could improve on is that the 4.9% measurement that I used in my hypothesis testing was given to me by my source and was not calculated by me directly.
    
-   **Future Work**
    1. To remedy shortcoming 1, I can further build a linear regression model with the existing data and use it to be able to forecast what the data should look like if it were extended to the current year 2024. Then I can continue this process year after year and update my models and datasets accordingly to keep up with the times and be appropriately ahead of the current times.

    2. To remedy shortcoming 2, I can instead find a dataset with more specific categories and items that I can give even more detailed observations on not only how GDP and Education Budget are related, but what sections within the Education Budget are best funded in relation to GDP growth.
    
    3. In order to remedy shortcoming 3, I would like to follow up by finding a dataset on the ratio of GDP to education budgets for each year of the OECD organization for a period of time, and then set its mean to $\bar{x_2}$, changing my hypothesis testing for a single mean to hypothesis testing for a difference of two means, which will make the results more convincing.

#### Conclusion

In summary, I have found a positive linear relationship between GDP and the education budget for a given year. It is confident that the average GDP to education budget ratio of the United States exceeds the average ratio of OECD member countries. For answering Q1, I calculated my correlation coefficient and I obtained my answer to Q2 by creating a hypothesis test of the sample mean. Although the results I have obtained are considerable, there are still some improvements that can be made to obtain a more precise and in-depth analysis.

### References

<a id="ref-1"></a>1. OECD. *Education Financing*. 2021. <https://www.oecd.org/en/topics/education-financing.html>. [↩](#cite-1)

<a id="ref-2"></a>2. DataHub. *United States of America education budget analysis*. 2024. <https://datahub.io/core/usa-education-budget-analysis>. [↩](#cite-2)
