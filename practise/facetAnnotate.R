
df = mtcars
df[, c("cyl", "am", "gear")] <- lapply(df[, c("cyl", "am", "gear")], as.factor)

p <- ggplot(mtcars, aes(mpg, wt, group = cyl)) +   geom_line(aes(color=cyl)) +  geom_point(aes(shape=cyl)) +   facet_grid(gear ~ am)               
p
#long cut way to find number of facets
(len <- length(levels(df$gear)) *  length(levels(df$am)))

vars <- data.frame(expand.grid(levels(df$gear), levels(df$am)))
colnames(vars) <- c("gear", "am")
dat <- data.frame(x = rep(15, len), y = rep(5, len), vars, labs=LETTERS[1:len])
dat
p + geom_text(aes(x, y, label=labs, group=NULL),data=dat)

p + geom_text(x=20, y=5, aes(label=labs, group=NULL),data=dat)

df = mtcars
dfS1 <- df %>% group_by(cyl, gear,am) %>% summarise(n=n(), avgmpg = mean(mpg))
head(dfS1)
g1 <- ggplot(dfS1, aes(x=cyl, y=n, fill=gear)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_grid(am ~ .)
g1

(dfS2 <- df %>% group_by(am) %>% summarise(avgmpg = mean(mpg)))
dfS2$gear=factor(4)
g1 + geom_text(data=dfS2, x=1, y=5, aes(label=round(avgmpg))) + coord_flip()


df=ctable2Melt1
head(df)
g <- ggplot(df, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2), position = position_dodge2(.7)) + guides (fill=F) 
head(case1)
str(case1)
case1$variable = factor(case1$variable)
g
g + geom_text(y = Inf, x = 4, label='firstcase')
g + geom_text(data=case1, x=4, y=10, aes(label = paste('First Case - ', firstCase)), size=rel(2)) + coord_flip()
+ coord_flip()


#----
#facet - geom_hline
ggplot(UKWinners) +
  geom_point(aes(Pcode, TE.Contr.)) +
  geom_hline(data = dMean, aes(yintercept = MN)) +
  facet_wrap(~ Name)
dMean <- mtcars %>%  group_by(cyl) %>%  summarise(MN = mean(mpg))
ggplot(mtcars) +  geom_point(aes(cyl, mpg)) +  geom_hline(data = dMean, aes(yintercept = MN)) +  facet_wrap(~ cyl)

dMean <- mtcars %>%  group_by(cyl, gear) %>%  summarise(MN = mean(mpg))
ggplot(mtcars) +  geom_point(aes(cyl, mpg)) +  geom_hline(data = dMean, aes(yintercept = MN)) +  facet_wrap(gear ~ cyl)
