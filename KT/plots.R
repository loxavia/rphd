par(mar=rep(2,2,1,1))
#plot
#2D
plot(women)
#3D
plot(x= mtcars$wt, y= mtcars$mpg, col=mtcars$cyl)
#5D
#X-Weight, Y-Mileage, Color-Cylinder, Shape-Gear, Size-AM : Points
lapply(mtcars[,c('cyl','gear','am')], table)  #count of each category cols
#you should know this to plan how many colors, shapes, size to choose
plot(x= mtcars$wt, y= mtcars$mpg, col=c(1,2,3), pch=c(20,21,22), cex=c(1,2), xlab='Weight', ylab='Mileage', main='Weight vs Mileage')
legend("topright",legend=c("Cyl-4", "Cyl-6", "Cyl-8"),pch=10, col=1:3, title="Cylinder")
legend("topleft", inset=.02, legend=c("Gear-3", "Gear-4", "Gear-5"), pch=20:23, title="Gear")
legend("top", legend=c('Auto','Manual'), pch=c(20,21), pt.cex=c(1,1.5), title='Tx Type')
#pch has to be given for all legend. type, color and size as per legend


#boxplot----
#1D
boxplot(mtcars$mpg)
boxplot(mtcars$mpg, main = "Mileage Distribution", xlab = "MPG",      ylab = "mpg Variable", col = "orange", border = "brown", horizontal = TRUE,notch = TRUE)
#2D
#formula method
boxplot(mpg ~ cyl, data=mtcars, main = "Mileage Distribution", xlab = "MPG",      ylab = "Miles", col = 1:3, border = "brown", horizontal = TRUE,notch = TRUE)
#which car Cylinder Type has higher mean mileage and more range of mileage - Cyl4 


#histogram----
hist(mtcars$mpg, labels=T, main='Histogram of MPG')
hist(mtcars$mpg, col='green', labels=T,breaks=10) #approx 10 rectangles
range(mtcars$mpg)
hist(mtcars$mpg, breaks=c(10,15,20,25,30,40)) #rectangles at specific points
hist(mtcars$mpg, col=mtcars$am) #auto cars have
legend('topright',legend=c('Auto','Manual'), pch=10, col=1:2)
#1st color goes to auto(0), 2nd color goes to Manaul(1) : order 0,1
plot(density(mtcars$mpg))
#hist + density: make freq=F to match the scales
hist(mtcars$mpg, freq=F); lines(density(mtcars$mpg))

#barplot : Categorical Cols---
t1 = table(mtcars$cyl)
barplot(height=t1, col=1:3, main='Bar Plot', names.arg=c('Cyl1','Cyl2','Cyl3'))
(t2 = table(mtcars$cyl, mtcars$gear, dnn=c('cyl','gear')))
barplot(height=t2, col=1:3, main='Bar Plot : Cyl vs Gear')
barplot(height=t2, col=1:3, beside=T, horiz=T, main='Bar Plot : Cyl vs Gear')
barplot(height=t2,col=1:3, beside=T, horiz=T, main='Bar Plot : Cyl vs Gear')

#pie plot
t1
t2
pie(t1)
pie(t1, labels=c('Cyl4','Cyl6','Cyl8'), radius=1, col=1:3)
pie(t1, labels=paste(c('Cyl4','Cyl6','Cyl8'), ': Count : ', t1))
#Pie charts are a very bad way of displaying information.
#cannot differentiate between the pies..

#line plot
coySales = data.frame(year=sample(2001:2020, size=20, replace=F), sales=sample(50:100, size=20, replace=T))
head(coySales)  #year is not in order
plot(x=coySales$year, y=coySales$sales, type='l') #this is will be zig zag as year is not in order
coySalesOrder = coySales[order(coySales$year),]
plot(data=coySalesOrder, sales ~ year, type='b', col=2, lwd=2, main='Year - Sales')
text(data=coySalesOrder, sales ~ year, labels=sales, cex=0.9, font=2)



#Line Graph----
# Define the cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Graph the cars vector with all defaults
plot(cars)

# Graph cars using blue points overlayed by a line 
plot(cars, type="o", col="blue")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)

# Graph cars using a y axis that ranges from 0 to 12
plot(cars, type="o", col="blue", ylim=c(0,12))

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)

# Calculate range from 0 to max value of cars and trucks
g_range <- range(0, cars, trucks)
legend(1, g_range[2], c("cars","trucks"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2);

#Plotting lines for all three vectors: dataframe:autos_data:cars,trucks,suvs
# Compute the largest y value used in the data (or we could
# just use range again)
max_y <- max(autos_data)
max_y

# Define colors to be used for cars, trucks, suvs
plot_colors <- c("blue","red","forestgreen")

# specify them ourself
plot(autos_data$cars, type="o", col=plot_colors[1], axes = F, ann = F)
# Make x axis using Mon-Fri labels
axis(1, at=1:5, lab=c("Mon", "Tue", "Wed", "Thu", "Fri"))
# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:max_y is equivalent to c(0,4,8,12).
axis(2, las=1, at=4*0:max_y)

