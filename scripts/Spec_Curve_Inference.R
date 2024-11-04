# Simonsohn et al., 2020: 
# 1. transform p value to associated Z score 
#    "for example, Z = 1.96 for p = 0.05"
# 2. average Z scores
# 3. inverse transform average Z score to p value


# p to z ------------------------------------------------------------------
p = .05 #this will be a vector in final analysis
z = qnorm(p/2, lower.tail = FALSE) #to reproduce "for example, Z = 1.96 for p = 0.05"
# should be differentiated by whether p-value is one- or two-sided


# average z ---------------------------------------------------------------
z.mean = mean(z)


# z to p ------------------------------------------------------------------
p.mean = pnorm(z.mean, lower.tail = FALSE)*2 #contingent on reproduction of "for example, Z = 1.96 for p = 0.05"
