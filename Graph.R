x <- runif(50,0,2)
y <- runif(50,0,2)
plot(x, y, main="散点图", xlab="横坐标", ylab="纵坐标")
text(0.6,0.6,"text at (0.6,0.6)")
abline(h=.6,v=.6)



x <- rnorm(100) # 生成随机数
y <- rnorm(100) + x # 生成随机数
reg1 <- lm(x~y)
plot(x, y, main="Show Case")
abline(reg1)

