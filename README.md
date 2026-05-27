# Customer-Segmentation-Using-RFM-and-Clustering

## Overview
Online retail is a transactional data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail.The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.
## Dataset Information
- Source: https://archive.ics.uci.edu/dataset/352/online+retail
- Number of transactions: 541909
- Features include:
InvoiceNo
StockCode
Description
Quantity
InvoiceDate
UnitPrice
CustomerID
Country
## Methodology
### 1. Data preprocessing
- Removed missing values and canceled transactions
- Converted data types and transformed data format
- Filtered invalid quantities and prices
### 2. RFM Analysis
The following metrics were calculated for each customer:
- Recency (R): Number of days since last purchase
- Frequency (F): Number of transactions
- Monetary (M): Total spending amount
### 3. Data preparation for Clustering
- Treated outliers in RFM variables
- Standardized features using scaling
- Prepared the dataset for K-Means clustering
### 4. K-Means Clustering
- Used the Elbow Method and Average Silhouette Score to determine optimal k
- Applied K-Means clustering
### 5. Customer Segments
- Cluster 1	Active customers
- Cluster 2	At-risk / inactive customers
- Cluster 3	VIP loyal customers
## Technologies Used
R
tidyverse
dplyr
ggplot2
cluster
factoextra
## Results
The project successfully identified customer groups with different purchasing behaviors. The segmentation can help businesses:
- Improve customer retention
- Personalize marketing campaigns
- Identify high-value customers
- Optimize business strategies
