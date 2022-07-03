# Getting the same result as t.test()
# H0: mean(h) == mean(c)

rm(list=ls())

# length(h) != length(c)

h = c(10,14,14,16,17,18,18,19,20,20,21,21,23,24,29,29,30,30,31,31,31,33,35,37,40,41,42,42,44,46,48,49,51,53,54,54,55,56)

c = c(1,1,3,4,5,8,10,11,13,14,14,15,17,19,20,22,24,24,24,25,26,26,26,28,29,29,32,35,38,39,40,41,44,45,47,47,47,50,50,51)

# degrees of freedom (minus 2 because of mean(h) and mean(c))
dfe = length(h) + length(c) - 2

# standard error from pooled variance
sse = var(h) * (length(h) - 1) + var(c) * (length(c) - 1)
mse = sse / dfe
se = sqrt(mse * (1 / length(h) + 1 / length(c)))

# t statistic
t = (mean(h) - mean(c)) / se

# p-value
pval = 2 * (1 - pt(t,dfe))

# confidence interval
a = mean(h) - mean(c) - qt(0.975,dfe) * se
b = mean(h) - mean(c) + qt(0.975,dfe) * se

cat("t =",t,"df =",dfe,"p-value =",pval,"\n")
cat("95% confidence interval:\n",a,b,"\n")

print(t.test(h,c,var.equal=TRUE))

