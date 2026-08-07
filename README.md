# 📊 Customer Segmentation & Revenue Analytics

An end-to-end customer analytics project using **RFM Analysis and K-Means Clustering** to segment customers based on purchasing behavior and build interactive Power BI dashboards for revenue monitoring, customer value analysis, and churn-risk identification.

---

## 🎯 Project Overview

Understanding **how revenue changes over time** and **which customers drive business value** is essential for making data-driven decisions.

This project uses transaction-level data from the **Online Retail dataset** to transform raw sales transactions into actionable customer insights.

The analysis combines:

- Data Cleaning & Preprocessing
- Exploratory Data Analysis
- RFM Analysis
- K-Means Clustering
- Customer Segmentation
- Revenue Analysis
- Customer Churn-Risk Analysis
- Interactive Power BI Dashboards

The project ultimately answers two key business questions:

> **1. How is revenue changing over time?**

> **2. Who are the most valuable customers, and who is at risk of leaving?**

---

# 🗂️ Dataset

The project uses the **Online Retail dataset**, containing transactional records from an online retail business.

### Main Variables

| Variable | Description |
|---|---|
| `InvoiceNo` | Unique transaction/invoice identifier |
| `StockCode` | Unique product code |
| `Description` | Product description |
| `Quantity` | Quantity purchased |
| `InvoiceDate` | Transaction date and time |
| `UnitPrice` | Price per unit |
| `CustomerID` | Unique customer identifier |
| `Country` | Customer's country |

A new metric was created to measure transaction-level revenue:

```text
Revenue = Quantity × UnitPrice
