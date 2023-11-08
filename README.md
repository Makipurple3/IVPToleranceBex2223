IVP Bex 2223 MAungKyaw
---
title: "BEX 223 MAungKyaw"
output: pdf_document
---

```{r setup, include=FALSE}
# Set global knitr options
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction


## 0.Opening the data
### Loading data

```{r LoadData, echo= FALSE}

# Load knitr for  markdown outputs
library(knitr)

##Load pander for better presentation of the outputs
library(pander)

##Load dplyr for better manipulation of the data
library(dplyr)

# Load readxl package to import the data
library("readxl")

# Read xlsx files
Boxex <- read_xls("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis MAung Kyaw/Box Experiments.xls")

``` 

* First I downloaded the **knitr package** in order to create some r markdown outputs as html, pdf or word files and the **pander** package for better presentation
* Then, I installed the **readxl package** to import my dataset which is called **Box Experiments.xls**

## 1.Explore the data

### First look at the dataset

```{r Glimps of Boxex, echo=FALSE}

#GLimps function to see the summary of the structure

cat("Glimpse of the Data Frame:\n\n")
glimpse(Boxex)
```
```



* I am now using the **view** and **head** function to see my whole dataset and to print a small example of it for the rmd output


# Annex

#### Annex 1 : View of the dataset when imported - First 6 entries of each variable 
* We can see here the brief view of the **original dataset** names **BoxEx**when i initially imported it as seen in **section 0: Opening data** 
```{r View of the data Boxex, echo=FALSE}

# Display the original dataset that is name Boxex
pander(head(Boxex), style = "rmarkdown", caption = "First Few Entries") 

```
