# 2.1 (b)
library(readr)

df = read.csv("O:/Arr Matey/Hwk-data2.csv", header=T, na.strings="?")
df = na.omit(df)

baseline_before = df$BMI_baseline_never_smoking_women
baseline_after = df$BMI_6year_follow_up_never_smoking_women
smokers_before = df$BMI_baseline_heavy_smoking_women
smokers_after = df$BMI_6years_after_quitting_heavy_smoking_women

t.test(smokers_before, smokers_after, paired=TRUE)


### 2.1 (c) ###
smoker_difference = smokers_after - smokers_before
shapiro.test(smoker_difference)
# W = 0.9138, p-value = 0.3081; hence normal

t.test(smoker_difference, conf.level=0.98)$"conf.int"
# 1.162738 5.557262


### 2.1 (d) ###
baseline_difference = baseline_after - baseline_before
var.test(smoker_difference, baseline_difference)

### 2.1 (e) ###
t.test(smoker_difference, baseline_difference, var.equal=TRUE)

### 2.1 (f) ###
t.test(smoker_difference, baseline_difference, var.equal=TRUE, conf.level=.9)$"conf.int"
