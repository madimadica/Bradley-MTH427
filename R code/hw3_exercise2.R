library(readr)

df = read.csv("O:/Arr Matey/Credit.csv", header=T, na.strings="?")
df = na.omit(df)

balance = df$Balance;
income = df$Income;
limit = df$Limit;
education = df$Education;
rating = df$Rating;

multi_model = lm(balance ~ income + limit + education + rating);
summary(multi_model)

confint(multi_model, level=0.95);

smaller_model = lm(balance ~ income + rating);
summary(smaller_model)

#2 h 
k = 4
g = 2
n = 400

ssec = (n-k-1)*(162.4^2)
sser = (n-(k-g)-1)*(162.9^2)


numerator = (sser - ssec) / (k-g)
denominator = ssec / (n-k-1)
F = numerator/denominator
F

pf(F, 2, n-k-1, lower.tail=FALSE)
