library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(DataExplorer)

student_data <- read_csv("C:/Users/Yacoub/Desktop/Dossier_Evaluation - 2025/StudentPerformanceFactors.csv")
head(student_data)
summary(student_data)
str(student_data)

colSums(is.na(student_data))
student_data <- na.omit(student_data)
colSums(is.na(student_data))

graphics.off()

numeric_vars <- select_if(student_data, is.numeric)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))  
for (var in colnames(numeric_vars)) {
  boxplot(numeric_vars[[var]], main = var, col = "green", horizontal = TRUE)
}
par(mfrow = c(1, 1))

outlier_vars <- c("Exam_Score", "Hours_Studied", "Tutoring_Sessions")
for (var in outlier_vars) {
  Q1 <- quantile(student_data[[var]], 0.25)
  Q3 <- quantile(student_data[[var]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  student_data <- student_data %>% filter(.data[[var]] >= lower_bound & .data[[var]] <= upper_bound)
}

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))  # Ajustement des marges
for (var in outlier_vars) {
  boxplot(student_data[[var]], main = var, col = "lightgreen", horizontal = TRUE)
}
par(mfrow = c(1, 1))

  
hist(student_data$Exam_Score, 
     main = "Distribution des Scores aux Examens", 
     col = "green", 
     xlab = "Score", 
     border = "black", 
     breaks = 20)


numeric_vars <- select_if(student_data, is.numeric)
cor_matrix <- cor(numeric_vars)

corrplot(cor_matrix, method = "color", tl.cex = 0.7, addCoef.col = "black")


ggplot(student_data, aes(x = Exam_Score, fill = Motivation_Level)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densité des Scores par Niveau de Motivation", x = "Score", y = "Densité")


ggplot(student_data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relation entre Heures d'Étude et Score d'Examen")

ggplot(student_data, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution des Scores des Élèves", x = "Score", y = "Nombre d'élèves")



ggplot(student_data, aes(x = Motivation_Level, y = Exam_Score, fill = Motivation_Level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Impact de la Motivation sur les Scores")


ggplot(student_data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Relation entre les Heures d'Étude et les Scores", x = "Heures d'Étude", y = "Score à l'Examen")

ggplot(student_data, aes(x = Tutoring_Sessions, y = Exam_Score)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Effet des Sessions de Tutorat sur les Scores", x = "Sessions de Tutorat", y = "Score à l'Examen")


