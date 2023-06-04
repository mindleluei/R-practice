



#상관계수 박스 플롯을 보면 mpg와 disp에 각각 가장 높은 음의 상관계수와 양의 상관계수가
#있음을 알 수 있다
#보면 mpg와 wt간 가장 큰 음의 상관관계가 있고,
#disp 와 cyl 간 가장 큰 양의 상관관계가 있다.



mtcars <- datasets::mtcars

model1 <- lm(mpg ~ wt, data= mtcars)
summary(model1)
plot(mpg ~ wt, data = mtcars)
abline(model1)

model2 <- lm(disp ~ cyl, data = mtcars)
summary(model2)
plot(disp ~ cyl, data = mtcars)
abline(model2)


cor(mtcars)



#가장 설명력 높은코드
model2 <- lm(mpg ~ wt, data = mtcars)
summary(model2)
plot(mpg ~ wt, data = mtcars)
abline(model2)


model3 <- lm(mpg ~ drat, data = mtcars)


#기술 통계 분석, 시각화
str(mtcars); head(mtcars) #데이터 구조
summary(mtcars) #요약통계량

#중심성향 파악
boxplot(mtcars)
boxplot(mtcars[,-c(3,4)]) #큰 수 제외
boxplot(mtcars[,c(3,4)]) #큰 수만
#범주형 자료는 박스플롯으로 확인하는데 어려움이 있음.
#다른 분석 방식을 써줌.

#산포



#his, boxplot, Plot = 연속형 자료
#barplot, dotchart = 이산형 자료, cyl, carb, gear
#pie, barplot, mosaicplot(다변량) = 범주형 자료

#범주형 자료 빈도 파악, barplot, piechart
table(mtcars$vs)
table(mtcars$am)
?mtcars

#범주형 자료 barplot & piechart

barplot(table(mtcars$vs),
        main = 'vs',
        col = c('darkblue', 'red'),
        ylim = c(0,20),
        legend.text  = c("0 = Straight", "1 = V"))
        
barplot(table(mtcars$am),
        main = 'am',
        col = c('lightgreen','violet'),
        ylim = c(0,20),
        legend.text = c("0 = automatic", "1 = manual"))

dotchart(mtcars$vs, groups = mtcars$mpg, bg = c('red', 'white'))
dotchart(mtcars$am, groups = mtcars$am, bg = c('red', 'white'))


counts <- table(mtcars$vs, mtcars$gear); counts
barplot(counts,
        col = c("gold","blue"),
        legend = row.names(counts),
        xlim = c(0, 18), horiz = T,
        angle = 45,
        main = 'vs & gear',
        legend.text = c('vs', 'gear'))

counts3 <- table(mtcars$am, mtcars$gear)



?mtcars
#파이 차트
count_vs <- c(18,14)
count_am <- c(19,13)

pie((sort(table(mtcars$vs))),
         decreasing = TRUE,
         radius = 1,
         main = 'vs',
         col = c('white','lightblue'))
legend("topright", legend = c("0 = automatic", "1 = manual"), fill = c("white", "lightblue"))

pie((sort(table(mtcars$am))),
         decreasing = TRUE,
         radius = 1,
         main = 'am',
         col =c("white", "lightblue"),
         labels = paste0(count_am))
legend("topright", legend = c("0 = automatic", "1 = manual"), fill = c("white", "lightblue"))


?mtcars
#이산형 자료 barplot, dotchart
counts2 <- table(mtcars$cyl, mtcars$carb, mtcars$gear); counts2
par(mfrow = c(1,3))
dotchart(mtcars$cyl, group = mtcars$disp, main = "cyl",  labels = mtcars$cyl, bg = c("red","white"))
dotchart(mtcars$carb, main = 'carb', xlim = c(1,8), pch = 2)
dotchart(mtcars$gear, main = 'gear', pch = 10)


#연속형 자료 hist, boxplot, plot
num <- mtcars[,-c(2,3,4,8,9,10,11,12)] ;num
boxplot(mtcars[,-c(2,3,4,8,9,10,11,12)], main = 'plot') #큰 수치는 따로 비교
boxplot(mtcars[,c(3,4)], main = 'plot') #큰 수치
plot(mtcars[,-c(2,3,4,8,9,10,11,12)])
hist(num)
par(mfrow = c(1,2))
dotchart(mtcars$mpg,
         labels = rownames(mtcars),
         cex = 0.7,
         pch = 1,
         main = "Gas Milage for Car Models",
         xlab = "Miles Per Gallon")

grps = as.factor(mtcars$cyl)
color = c("black", "red", "blue")
mtcars$pch[mtcars$cyl == 4] = 15
mtcars$pch[mtcars$cyl == 6] = 16
mtcars$pch[mtcars$cyl == 8] = 17

dotchart(mtcars$mpg, labels = rownames(mtcars), groups=grps,
         gcolor = color, color = color[grps],
         cex = .7, pt.cex = 1, pch = mtcars$pch, xlba = "mpg",
         main = 'MPG by \n Number of Cylinders')
#실린더 수가 적을수록 주행거리가 늘어남!

boxplot(mtcars[,-c(3,4)])
#다중선형회귀분석
all_lm <- lm(mpg ~ ., data = mtcars)
summary(all_lm)
all_lm

