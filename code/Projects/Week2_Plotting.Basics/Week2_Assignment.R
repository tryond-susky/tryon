# With the data frame you created last week you will:
# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment - Forgot to do this.
# e.g. mine would be Wilson_Week1_Assignemnt

#The 6 vectors: sorry there is five salmon and i couldn't choose which one to delete so now i have 6 vectors

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

df[,-1]

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

df1$Coho <- as.numeric(1.1,1.2,1.3,1.4,1.5,2.1,2.2,2.3,2.4,2.5,3.1,3.2,3.3,3.4,3.5)
df1$Sockeye

df1.mean <- aggregate(FUN = "mean", df1$Coho ~df1$Sockeye)
df1.mean

colnames(df1.mean) <- c("Factor","Mean")
df1.mean

barplot(df1.mean$Mean)

barplot(df.mean$Mean, 'Fish Scales' = df.mean$Factor)
barplot(df1.mean$Mean, names.arg = df1.mean$Factor)

barplot(as.matrix(data))

df1.sd <- aggregate(df1$Coho ~df1$Sockeye, FUN = "sd")

colnames(df1.sd) <- c("Factor","StanDev")
df1.sd

b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)

arrows(b.plot, df1.mean$Mean-df1.sd$StanDev,
       b.plot, df1.mean$Mean+df1.sd$StanDev,angle=90,code=3)

b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5))

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5), horiz = TRUE)

####Scatterplot
# Create a scatter plot between two of your numeric columns.
# Change the point shape and color to something NOT used in the example.
# Change the x and y labels and add a title
# Export the plot as a JPEG by using the "Export" button in the plotting pane.
plot(df1$Chum ~ df1$King)

plot(df1$Chum ~ df1$King, xlab = "Time", ylab = "Length", main = "Pacific Salmon 2022 Run", pch = 8 , col = "dodgerblue2")

?pch
demo('colors')


# Upload both plots with the script used to create them to GitHub.
# Follow the same file naming format as last week for the script.
write.csv(df1, file = "Example.csv")

# Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.

