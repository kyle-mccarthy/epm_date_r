# import the grade data from the python script
library("ggplot2")
library("plyr")
library(scales)

grade_data = read.csv("clean_joined_grade_data.csv")

intermediate_grades_df = data.frame(sessions = c("Session 2", "Session 3", "Session 4",
                                                 "Session 5", "Session 6"),
                                    values = c(mean(grade_data$intermediate_session_2), 
                                               mean(grade_data$intermediate_session_3), 
                                               mean(grade_data$intermediate_session_4),
                                               mean(grade_data$intermediate_session_5),
                                               mean(grade_data$intermediate_session_6)))
int_plot = ggplot(intermediate_grades_df) + aes(x = sessions, y = values) +
  geom_bar(stat = "identity")

session_data = read.csv('clean_joined_session_activity_data.csv')

session_table = count(session_data, c('student_id', 'session_id'))
session_2_subset = subset(session_table, session_id == 2)

access_freq_df = data.frame(student_id = session_2_subset$student_id,
                            freq = session_2_subset$freq)

acess_freq_tbl = ggplot(access_freq_df, aes(x = student_id, y = freq)) + 
  geom_point() + scale_x_continuous(breaks = pretty_breaks(n=30))

session_2_grade_df = data.frame(student_id = grade_data$student_id,
                                grade = grade_data$intermediate_session_2)

joined_grades_freq_df = merge(x = access_freq_df, y = session_2_grade_df, 
                              by = "student_id")

acess_freq_grades_tbl = ggplot(joined_grades_freq_df, aes(x = student_id, y = freq)) + 
  geom_point(aes(color = grade)) +
  scale_x_continuous(breaks = pretty_breaks(n=30))
