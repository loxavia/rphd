library(data.table)
select(mtcars, .data$cyl)
?select

?dplyr::select
dplyr::select(mtcars, cyl)
dplyr::select(mtcars, .data$cyl)
dplyr::select(mtcars, 1:3)
dplyr::select(mtcars, c(cyl, gear))
dplyr::select(mtcars, c(1,4))

mtcars %>% group_by(cyl)  %>% summarise(countKanika = n())

aggregate(x=mtcars$mpg, by=list(mtcars$cyl) , FUN=mean)
aggregate(formula= mpg ~ cyl , data=mtcars, FUN=mean , subset=am==0)

aggregate(x=mtcars$mpg, by=list(mtcars$cyl, mtcars$gear) , FUN=mean)
aggregate(formula= mpg ~ cyl + gear , data=mtcars, FUN=mean , subset=am==0)
aggregate(x=cbind(mtcars$mpg, mtcars$wt), by=list(mtcars$cyl, mtcars$gear) , FUN=mean)
aggregate(formula= cbind(mpg, wt) ~ cyl + gear , data=mtcars, FUN=mean , subset=am==0)

df=mtcars[,c('wt','mpg','gear','cyl','am')]
aggregate(formula= cbind(mpg, wt) ~ . , data=df, FUN=mean, subset=mpg >22)
