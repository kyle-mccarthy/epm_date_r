library("ggplot2")
library("plyr")
library("e1071")
library("scales")

grade_data = read.csv("clean_joined_grade_data.csv")
session_data = read.csv('clean_joined_session_activity_data.csv')
student_profile_data = read.csv('student_grade_profiles.csv')

# we want to discard the students that have less than 0 for there best exam
# because that indicates that they did not complete the course
completed_profiles = subset(student_profile_data, best_exam > 0)

int_avg = completed_profiles$intermediate_avg
best_exam = completed_profiles$best_exam

int_avg_vs_best_exam = ggplot(completed_profiles, aes(
  x = int_avg, y = best_exam)) + geom_point() + geom_smooth(method=lm) +
  xlab('intermediate score averages') + 
  ylab('best exam score')

completed_r2 = cor(int_avg, best_exam)^2

# the r2 value was p
int_avg_2 = student_profile_data$intermediate_avg
best_exam_2 = student_profile_data$best_exam

ggplot(student_profile_data, aes(
  x = int_avg_2, y = best_exam_2)) + geom_point() + geom_smooth(method=lm) +
  xlab('intermediate score averages') + 
  ylab('best exam score') +
  scale_y_continuous(breaks = pretty_breaks(n=30))

including_withdrawn_r2 = cor(int_avg_2, best_exam_2)^2

## SVM Section
## http://rischanlab.github.io/SVM.html
model = svm(int_avg_2, best_exam_2)
pred = predict(model, int_avg_2)

## Grade Range Disribtuion
letter_grade = student_profile_data$best_exam_letter
ggplot(student_profile_data, aes(
  x = int_avg_2, y = letter_grade)) + geom_point() +
  xlab('intermediate score averages') + 
  ylab('best exam score')

student_profile_data_df = data.frame(student_profile_data)
sorted_students = arrange(student_profile_data_df, -best_exam)

ggplot(student_profile_data, aes(
  x = int_avg_2, y = as.numeric(cut(sorted_students, 20)))) + geom_point() +
  xlab('intermediate score averages') + 
  ylab('best exam score')


