# With the data frame you created last week you will:
# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment - Forgot to do this.
# e.g. mine would be Wilson_Week1_Assignemnt #Naming system...

#The 6 vectors: sorry there is five salmon and i couldn't choose which one to delete so now i have 6 vectors #fine...but you forgot masu and kokanee!

salmon <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
sockeye <- c('w','w','w','w','w','y','y','y','y','y','n','n','n','n','n')
king <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
pink <- c(42,43,56,78,98,87,76,9,56,45,57,32,34,24,42)
coho <- c(1.1,1.2,1.3,1.4,1.5,2.1,2.2,2.3,2.4,2.5,3.1,3.2,3.3,3.4,3.5)
chum <- c(2,4,6,8,9,10,12,14,16,18,20,22,24,26,28)

data <- cbind(salmon, chum, sockeye, king, pink, coho)
data

df <- as.data.frame(data)
df


colnames(df) <- c('Salmon', 'King','Pink','Chum','Coho','Sockeye')
df


row.names(df) <- df$Salmon
df

df[,-1] #this doesn't get rid of the column, just prints without the column. Doesn't really matter for this particular assignment, but will be an issue in the future.

add.row <- data.frame('p',29, 'n', 16, 78, 4.1)

colnames(add.row) <- colnames(df)

df1 <- rbind(df, add.row)
df1

colnames(df1) <- c('Salmon', 'King','Pink','Chum','Coho','Sockeye')
df

row.names(df1) <- df1$Salmon
df1

df1[,-1]


# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
# Add error bars with mean and standard deviation to the plot
# Change the x and y labels and add a title
# Export the plot as a PDF that is 4 inches wide and 7 inches tall.

?aggregate
df$Coho <- as.numeric(as.character(df$Coho))

df.mean <- aggregate(Coho ~Pink, data = df, FUN = "mean")
df.mean


colnames(df.mean) <- c("Factor","Mean")
df.mean

barplot(df.mean$Mean)

barplot(df.mean$Mean, names.arg = df.mean$Factor)

df.sd <- aggregate(df$Coho ~df$Pink, FUN = "sd")

colnames(df.sd) <- c("Factor","StanDev")

df.sd

b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,100))

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,100), xlab = "Time", ylab = "Length", main = "Pacific Salmon 2022 Run", col = "dodgerblue2")


#Where is the code to export this as a pdf with specific dimensions? 
#Exported plot is missing error bars
#Did not set working directory

####Scatterplot
# Create a scatter plot between two of your numeric columns.
# Change the point shape and color to something NOT used in the example.
# Change the x and y labels and add a title
# Export the plot as a JPEG by using the "Export" button in the plotting pane.
plot(df1$Chum ~ df1$King)

plot(df1$Chum ~ df1$King, xlab = "Time", ylab = "Length", main = "Pacific Salmon 2022 Run", pch = 8 , col = "dodgerblue2") #Was supposed to be exported as a JPEG.

?pch
demo('colors')


# Upload both plots with the script used to create them to GitHub.
# Follow the same file naming format as last week for the script. #Didn't follow the naming format.

write.csv(df, file="week 2 assignment.csv") #Didn't need to do this.
getwd()


