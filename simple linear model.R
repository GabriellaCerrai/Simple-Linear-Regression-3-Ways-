data = read.csv('real_estate_price_size_year.csv', header = TRUE)
head(data)
attach(data)
options(scipen=999)

model = lm(price~size+year, data = data)
summary(model)


# creating own summary table: 
coefs = round(model$coefficients, 3)
coefs
summary(model)$r.squared
summary(model)$adj.r.squared

table = matrix(NA, ncol = 1, nrow = 5)
table[1,] = coefs[1]
table[2,] = coefs[2]
table[3,] = coefs[3]
table[4,] = round(summary(model)$r.squared,3)
table[5,] = round(summary(model)$adj.r.squared,3)
rownames(table) = c('Interept Coefficient','Size Coefficient','Year Coefficient','R2','Adjusted R2')
colnames(table) = c('Value')
table

# using a simple loop to populate the table 
table2 = matrix(NA, ncol = 1, nrow = 3)
colnames(table2) = c('Value')
rownames(table2) = c('Intercept Coefficient','Size Coefficient ','Year Coefficient')
for (i in seq(1,3))
{
  table2[i,] = coefs[i]
}
table2

