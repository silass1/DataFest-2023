#Read in data
library(readr)
attorneys <- read_csv("MIZ/DataFest2023/attorneys.csv")
attorneytimeentries <- read_csv("MIZ/DataFest2023/attorneytimeentries.csv")
categories <- read_csv("MIZ/DataFest2023/categories.csv")
clients <- read_csv("MIZ/DataFest2023/clients.csv")
questionposts <- read_csv("MIZ/DataFest2023/questionposts.csv")
questions <- read_csv("MIZ/DataFest2023/questions.csv")
statesites <- read_csv("MIZ/DataFest2023/statesites.csv")
subcategories <- read_csv("MIZ/DataFest2023/subcategories.csv")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
questions$Category <- as.factor(questions$Category)

#converting date variables into seconds
library(tidyverse)
library(lubridate)

questions <- questions|>
  mutate(ClosedOnUtc=as_datetime(ClosedOnUtc)) |>
  mutate(ClosedOnUtc = as.numeric(ClosedOnUtc, units = "seconds"))

questions <- questions|>
  mutate(AskedOnUtc=as_datetime(AskedOnUtc)) |>
  mutate(AskedOnUtc = as.numeric(AskedOnUtc, units = "seconds"))

questions <- questions|>
  mutate(LegalDeadline=as_datetime(LegalDeadline)) |>
  mutate(LegalDeadline = as.numeric(LegalDeadline, units = "seconds"))

questions <- questions|>
  mutate(TakenOnUtc=as_datetime(TakenOnUtc)) |>
  mutate(TakenOnUtc = as.numeric(TakenOnUtc, units = "seconds"))

na.omit(questions)

#converting seconds into days
questions$ClosedOnUtc = questions$ClosedOnUtc / 86400
questions$AskedOnUtc = questions$AskedOnUtc / 86400
questions$LegalDeadline = questions$LegalDeadline / 86400
questions$TakenOnUtc = questions$TakenOnUtc / 86400


#creating difference variables
questions$ClosedAskedDiff = questions$ClosedOnUtc - questions$AskedOnUtc
questions$TakenAskedDiff = questions$TakenOnUtc - questions$AskedOnUtc
questions$ClosedTakenDiff = questions$ClosedOnUtc - questions$TakenOnUtc
questions$DeadlineAskedDiff = questions$LegalDeadline - questions$AskedOnUtc


#Everything indexed to 2016
questions$ClosedOnUtc = questions$ClosedOnUtc -17018
questions$AskedOnUtc = questions$AskedOnUtc -17018
questions$TakenOnUtc = questions$TakenOnUtc -17018
questions$LegalDeadline = questions$LegalDeadline -17018

#Cleaning 2016-2022 with dplyr
questions2<- questions
filtered =questions2 %>%
  dplyr::filter(between(LegalDeadline, 0, 5114)) %>%
  dplyr::filter(between(ClosedOnUtc, 0, 5114)) %>%
  dplyr::filter(between(AskedOnUtc, 0, 5114)) %>%
  dplyr::filter(between(TakenOnUtc, 0, 5114)) %>%
  mutate(legal =  ifelse(LegalDeadline >= 0 & LegalDeadline <= 5114, T, F))

#Summary statistics of all created variables
install.packages("vtable")
library(vtable)
st(filtered)

library(ggplot2)

table(questions$Category)
prop.table(table(questions$Category))

# Time between Asked and Closed (Days)

print(tapply(X=filtered$ClosedAskedDiff, INDEX=list(filtered$Category), 
             FUN=mean , na.rm = TRUE))

# Create a sample dataset
data <- data.frame(x = c("Consumer Financial Questions", "Education", "Family and Children", 
                         "Health and Disability", " Housing and Homelessness", "Income Maintenance",
                         "Individual Rights", "Juvenile", "Work, Employment and Unemployment",
                         "Other"), y = c(12.80818, 17.12970, 13.03611,  16.41372 , 12.18250,
                                         17.60484, 13.60860, 16.17234, 14.64821,  11.87781))


# Define custom color palette
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "#999999", 
               "#F8766D", "#00BFC4")


# Create a bar graph with custom colors
ggplot(data, aes(x = x, y = y, fill = x)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Mean", title = "Time Between Question Asked and Closed (Days)") +
  theme(axis.text.x = element_blank())

###############################################
# Time between Asked and Taken  (Days)
print(tapply(X=filtered$TakenAskedDiff, INDEX=list(filtered$Category), 
             FUN=mean , na.rm = TRUE))
data1 <- data.frame(x = c("Consumer Financial Questions", "Education", "Family and Children", 
                          "Health and Disability", " Housing and Homelessness", "Income Maintenance",
                          "Individual Rights", "Juvenile", "Work, Employment and Unemployment",
                          "Other"), y = c(4.922850, 10.849707 , 6.739809  ,  8.696806 , 5.091293,
                                          10.386577 , 6.709524 ,  8.226220,  7.171859,  5.733352 ))


# Define custom color palette
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "#999999", 
               "#F8766D", "#00BFC4")


# Create a bar graph with custom colors
ggplot(data1, aes(x = x, y = y, fill = x)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Mean", title = "Time Between Question Taken and Asked (Days)") +
  theme(axis.text.x = element_blank()) 
########################################################################
# Time between Taken  and Closed (Days)
print(tapply(X=filtered$ClosedTakenDiff, INDEX=list(filtered$Category), 
             FUN=mean , na.rm = TRUE))
data2 <- data.frame(x = c("Consumer Financial Questions", "Education", "Family and Children", 
                          "Health and Disability", " Housing and Homelessness", "Income Maintenance",
                          "Individual Rights", "Juvenile", "Work, Employment and Unemployment",
                          "Other"), y = c(7.885326,6.279994 , 6.296297 ,   7.716911 , 7.091207 ,
                                          7.218266  ,6.899071,    7.946120,   7.476351,   6.144454))


# Define custom color palette
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "#999999", 
               "#F8766D", "#00BFC4")


# Create a bar graph with custom colors
ggplot(data2, aes(x = x, y = y, fill = x)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Mean", title = "Time Between Date Taken and Closed (Days)") +
  theme(axis.text.x = element_blank())
###################################################################################
# Time between Asked and Legal Deadline  (Days)
print(tapply(X=filtered$DeadlineAskedDiff, INDEX=list(filtered$Category), 
             FUN=mean , na.rm = TRUE))
data3 <- data.frame(x = c("Consumer Financial Questions", "Education", "Family and Children", 
                          "Health and Disability", " Housing and Homelessness", "Income Maintenance",
                          "Individual Rights", "Juvenile", "Work, Employment and Unemployment",
                          "Other"), y = c(19.16512  , 19.11546  , 22.15938 ,    27.79809, 12.13660,
                                          21.15734  , 25.78684 , 19.75228 ,  16.50274  ,21.39122 ))


# Define custom color palette
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "#999999", 
               "#F8766D", "#00BFC4")


# Create a bar graph with custom colors
ggplot(data3, aes(x = x, y = y, fill = x)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Mean", title = "Time Between Asked and Legal Deadline (Days)") +
  theme(axis.text.x = element_blank())



#cleaning 2016-2022 with dplyr
a =questions2 %>%
  dplyr::filter(between(LegalDeadline, 0, 5114)) %>%
  dplyr::filter(between(ClosedOnUtc, 0, 5114)) %>%
  dplyr::filter(between(AskedOnUtc, 0, 5114)) %>%
  dplyr::filter(between(TakenOnUtc, 0, 5114)) %>%
  mutate(legal =  ifelse(LegalDeadline >= 0 & LegalDeadline <= 5114, T, F))

filtered <- a

table(filtered$Category)
b <- prop.table(table(filtered$Category))


#creating difference variables
filtered$ClosedAskedDiff = filtered$ClosedOnUtc - filtered$AskedOnUtc
filtered$TakenAskedDiff = filtered$TakenOnUtc - filtered$AskedOnUtc
filtered$ClosedTakenDiff = filtered$ClosedOnUtc - filtered$TakenOnUtc
filtered$DeadlineAskedDiff = filtered$LegalDeadline - filtered$AskedOnUtc


cat1 <- filtered |> group_by(Category) |> summarise(mean_dead = mean(LegalDeadline)) |>  
  mutate(cat_weights = b) |>
  mutate(w_mean_legal_dead = cat_weights * mean_dead) 


cat2 <- filtered |> group_by(Category) |> summarise(mean_ca_diff = mean(ClosedAskedDiff))|>  
  mutate(cat_weights = b) |>
  mutate(w_mean_ca_diff = cat_weights * mean_ca_diff)

cat3 <- filtered |> group_by(Category) |> summarise(mean_ta_diff = mean(TakenAskedDiff))|>  
  mutate(cat_weights = b) |>
  mutate(w_mean_ta_diff = cat_weights * mean_ta_diff)

cat4 <- filtered |> group_by(Category) |> summarise(mean_ct_diff = mean(ClosedTakenDiff))|>  
  mutate(cat_weights = b) |>
  mutate(w_mean_ct_diff = cat_weights * mean_ct_diff)

cat5 <- filtered |> group_by(Category) |> summarise(mean_ld_diff = mean(DeadlineAskedDiff))|>  
  mutate(cat_weights = b) |>
  mutate(w_mean_ld_diff = cat_weights * mean_ld_diff)

cat_dupes <- cbind(cat2, cat3, cat4, cat5)
cat <- cat_dupes[!duplicated(as.list(cat_dupes))]

#weighted graphs
library(ggplot2)
ggplot(cat, aes(x = data$x, y = w_mean_ca_diff, fill = data$x)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Weighted Mean", title = "Weighted Difference Between Closed and Asked") +
  theme(axis.text.x = element_blank())

ggplot(cat, aes(x = data$x, y = w_mean_ta_diff, fill = data$x)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Weighted Mean", title = "Weighted Difference Between Taken and Asked") +
  theme(axis.text.x = element_blank())

ggplot(cat, aes(x = data$x, y = w_mean_ct_diff, fill = data$x)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Weighted Mean", title = "Weighted Difference Between Closed and Taken") +
  theme(axis.text.x = element_blank())


ggplot(cat, aes(x = data$x, y = w_mean_ld_diff, fill = data$x)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Weighted Mean", title = "Weighted Difference Between Asked and Deadline") +
  theme(axis.text.x = element_blank())




























