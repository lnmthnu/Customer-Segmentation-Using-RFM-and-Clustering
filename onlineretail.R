# A. Data preprocessing
## 1. Import required library and read the dataset
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(caret)
library(cluster)
library(hopkins)
library(fpc)
library(factoextra)

data <- read.csv("D:\\DA\\prj\\Customer Segmentation Using RFM and Clustering\\OnlineRetail.csv", na.strings = c("", "NA"), stringsAsFactors = FALSE)
View(data)

## 2. Exploration
# 10 dong dau tien cua du lieu
head(data)
# cau truc tap du lieu
str(data)
# cac chi so thong ke mo ta
summary(data)

## 3. Data cleaning
### 3.1. Missing value
# dem so mising value theo tung cot
colSums(is.na(data))

# ti le % missing value trong customerID
mean(is.na(data$CustomerID)) * 100

# xoa missing value o cọt description và customerID
data_clean <- data %>%
  filter(!is.na(Description), !is.na(CustomerID))

colSums(is.na(data_clean))

### 3.2. Transform data format
data_clean <- data_clean %>%
  mutate(
    InvoiceDate = dmy_hm(InvoiceDate),
    Date = as.Date(InvoiceDate),           # Chỉ lấy phần ngày
    Time = format(InvoiceDate, "%H:%M")    # Chỉ lấy phần giờ:phút
  )
# kiem tra
head(data_clean$InvoiceDate)

### 3.3. Filter invalid prices/quantities and remove cancellation values
data_clean <- data_clean %>%
  filter(UnitPrice > 0, Quantity > 0) %>%
  filter(!grepl("^C", InvoiceNo)) %>%
  mutate(CustomerID = as.integer(CustomerID))  

### 3.4. Convert data types
# kiem tra cac du lieu khong dung dinh dang cua  du lieu
check <- data_clean %>%
  filter(!grepl("^[0-9]{5}[A-Za-z]*$", StockCode)) %>%
  count(StockCode) %>%
  arrange(desc(n))
check

# loc cac du lieu khong dung dinh dang
data_clean <- data_clean %>%
  filter(grepl("^[0-9]{5}[A-Za-z]*$", StockCode))

### 3.5. Final Check
str(data_clean)

#### Quantity
summary(data_clean$Quantity)

quantity <- ggplot(data_clean, aes(x = Quantity)) +
  geom_histogram(bins = 50, fill = "#3498DB") + scale_x_log10() +
  labs(title = "Distribution of Quantity")
quantity

#### Unitprice
summary(data_clean$UnitPrice)

unitprice <- ggplot(data_clean, aes(x = UnitPrice)) +
  geom_histogram(bins = 50, fill = "#3498DB") + scale_x_log10() +
  labs(title = "Distribution of Unitprice")
unitprice

### 3.6. Save cleaned data as CSV file
write.csv(
  data_clean,
  "D:\\DA\\prj\\Customer Segmentation Using RFM and Clustering\\online_retail_cleaned.csv",
  row.names = FALSE
)

### 3.7. Create Cohort retention
cohort_data <- data_clean %>%
  filter(!is.na(CustomerID)) %>%
  mutate(InvoiceMonth = floor_date(as.Date(InvoiceDate), "month")) %>%
  group_by(CustomerID) %>%
  mutate(CohortMonth = min(InvoiceMonth)) %>%
  ungroup() %>%
  mutate(PeriodNumber = interval(CohortMonth, InvoiceMonth) %/% months(1))

cohort_counts <- cohort_data %>%
  distinct(CohortMonth, PeriodNumber, CustomerID) %>%
  group_by(CohortMonth, PeriodNumber) %>%
  summarise(ActiveCustomers = n(), .groups = "drop")

cohort_sizes <- cohort_counts %>%
  filter(PeriodNumber == 0) %>%
  select(CohortMonth, CohortSize = ActiveCustomers)

cohort_retention <- cohort_counts %>%
  left_join(cohort_sizes, by = "CohortMonth") %>%
  mutate(RetentionRate = ActiveCustomers / CohortSize)

write.csv(cohort_retention, "cohort_retention.csv", row.names = FALSE)

# B. RFM Analysis
## 1. Monetary
# tao bien Revenue
reference_date <- max(data_clean$Date) + days(1)
data_clean <- data_clean %>%
  mutate(Revenue = Quantity * UnitPrice)

# tính tổng doanh thu của từng khách hàng đã đóng góp cho công ty
rfm_m <- data_clean %>%
  group_by(CustomerID) %>%
  summarise(
    Monetary = sum(Revenue, na.rm = TRUE)
  )
head(rfm_m)

## 2. Recency
rfm_r <- data_clean %>%
  mutate(Diff = as.numeric(reference_date - Date)) %>% # tính khoảng cách giữa transaction data với reference date
  group_by(CustomerID) %>%
  summarise(Diff = min(Diff, na.rm = TRUE)) # tính ngày giao dịch gần nhất -> recency
head(rfm_r)

## 3. Fequency
rfm_f <- data_clean %>%
  group_by(CustomerID) %>%
  summarise(Frequency = n_distinct(InvoiceNo)) # đếm số đơn hàng thục tế
head(rfm_f)

###  Gộp thành 1 bảng RFM table
rfm <- rfm_m %>%
  inner_join(rfm_f, by = "CustomerID") %>%
  inner_join(rfm_r, by = "CustomerID") %>%
  rename(Recency = Diff)
head(rfm)

# C. Data preparation for Clustering
## 1. Outlier 
# chuyển về dạng long format
rfm_long <- rfm %>%
  select(Monetary, Frequency, Recency) %>%
  pivot_longer(cols = everything(), 
               names_to = "Attributes", 
               values_to = "Range")

# boxplot
ggplot(rfm_long, aes(x = Attributes, y = Range, fill = Attributes)) +
  geom_boxplot(width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Outliers Variable Distribution",
    x = "Attributes",
    y = "Range"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title  = element_text(face = "bold")
  )

#### Nhìn vào biểu đồ Boxplot, thấy có xuất hiện outlier -> thực tế cho thấy có các khách hàng mua hàng vượt trội -> thay vì xóa, sử dụng kỹ thuật outlier capping (giới hạn ngoại lệ)
cap_outliers <- function(x, upper_pct = 0.99, lower_pct = 0.01) {
  upper <- quantile(x, upper_pct, na.rm = TRUE)
  lower <- quantile(x, lower_pct, na.rm = TRUE)
  x <- ifelse(x > upper, upper, x)
  x <- ifelse(x < lower, lower, x)
  return(x)
}

rfm_clean <- rfm %>%
  transmute(
    CustomerID = CustomerID,
    Recency_capped = cap_outliers(rfm$Recency),
    Frequency_capped = cap_outliers(rfm$Frequency),
    Monetary_capped = cap_outliers(rfm$Monetary)
  )
rfm_clean

## 2. Rescaling: su dung chuan hoa Z(0,1)
rfm_clean1 <- preProcess(rfm_clean[2:4], method = c("center", "scale"))
rfm_scaled <- predict(rfm_clean1, rfm_clean[2:4])

summary(rfm_scaled) # mean = 0
var(rfm_scaled$Monetary_capped)
var(rfm_scaled$Frequency_capped)
var(rfm_scaled$Recency_capped) # var = 1

head(rfm_scaled)


## 3. Hopkins
hopkins <- get_clust_tendency(rfm_scaled, n=nrow(rfm_scaled)-1, graph = FALSE)
hopkins

# D. K-means Clustering
## 1. Determine optimal k
### WSS min (Elbow method)
fviz_nbclust(rfm_scaled, kmeans, method = "wss", k.max = 10) + theme_minimal()

### Silhouette Score
fviz_nbclust(rfm_scaled[, c("Recency_capped","Frequency_capped","Monetary_capped")], 
             kmeans, method = "silhouette") +
  labs(title = "Silhouette Method")

## 2. Using K-means clustering
# model
kmeans_model <- kmeans (rfm_scaled, centers=3, nstart=10)

# Xem tâm cụm
kmeans_model$centers

# đánh giá chất lượng phân tích cụm 
k_stats <- cluster.stats(dist(rfm_scaled), kmeans_model$cluster)
k_stats

# bss/tss
kmeans_model$betweenss / kmeans_model$totss

## 3. Final Analysis
### 3.1. Assign the label
rfm_clean$Cluster_Id <- kmeans_model$cluster
head(rfm_clean)

### 3.2. Cluster plot
plot_kmeans <- fviz_cluster(kmeans_model, data = rfm_scaled, geom = "point", eclipse.type = "convex", repel = TRUE)
plot_kmeans

plot_ly(rfm_clean, 
        x = ~Recency_capped, y = ~Frequency_capped, z = ~Monetary_capped,
        color = ~as.factor(Cluster_Id),
        colors = c('#E74C3C', '#3498DB', '#2ECC71'),  # tùy chỉnh màu theo số cluster
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 3)) %>%
  layout(title = "3D Cluster Plot - RFM Segmentation",
         scene = list(
           xaxis = list(title = "Recency"),
           yaxis = list(title = "Frequency"),
           zaxis = list(title = "Monetary")
         ))

### 3.3. Descriptive statistics
cluster_summary <- rfm_clean %>%
  group_by(Cluster_Id) %>%
  summarise(
    Monetary_mean = mean(Monetary_capped),
    Monetary_std = sd(Monetary_capped),
    Monetary_med = median(Monetary_capped),
    Frequency_mean = mean(Frequency_capped),
    Frequency_median = median(Frequency_capped),
    Frequency_std = sd(Frequency_capped),
    Recency_mean = mean(Recency_capped),
    Recency_std = sd(Recency_capped),
    Recency_median  = median(Recency_capped)
  )
cluster_summary

### 3.4. Box plot
# Reccency
rfm_recency <- cluster_summary %>%
  pivot_longer(cols = c(Recency_mean, Recency_std, Recency_median),
               names_to  = "Statistic",
               values_to = "Value")

ggplot(rfm_recency, aes(x = as.factor(Cluster_Id), y = Value)) +
  geom_boxplot() +
  labs(x = "Cluster_Labels", y = "Recency", fill = "Cluster") +
  theme(legend.position = "none")  

# Monetary
rfm_Monetary <- cluster_summary %>%
  pivot_longer(cols = c(Monetary_mean, Monetary_std, Monetary_med),
               names_to  = "Statistic",
               values_to = "Value")
ggplot(rfm_Monetary, aes(x = as.factor(Cluster_Id), y = Value)) +
  geom_boxplot() +
  labs(x = "Cluster_Labels", y = "Monetary", fill = "Cluster") +
  theme(legend.position = "none")

# Frequency
rfm_frequency <- cluster_summary %>%
  pivot_longer(cols = c(Frequency_mean, Frequency_median, Frequency_std),
               names_to  = "Statistic",
               values_to = "Value")
ggplot(rfm_frequency, aes(x = as.factor(Cluster_Id), y = Value)) +
  geom_boxplot() +
  labs(x = "Cluster_Labels", y = "Frequency", fill = "Cluster") +
  theme(legend.position = "none")

# F. Export segmented data for Power BI
# Gán tên Segment có ý nghĩa kinh doanh dựa trên kết quả phân tích
rfm_clean <- rfm_clean %>%
  mutate(
    Segment = case_when(
      Cluster_Id == 1 ~ "Lost/At risk",
      Cluster_Id == 2 ~ "Loyal Customers",
      Cluster_Id == 3 ~ "Champions"
    )
  )

# kiểm tra lại
table(rfm_clean$Segment)
head(rfm_clean)

# Export ra CSV 
write.csv(
  rfm_clean,
  "D:\\DA\\prj\\Customer Segmentation Using RFM and Clustering\\customer_segments.csv",
  row.names = FALSE
)