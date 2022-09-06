# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment
# e.g. mine would be Wilson_Week1_Assignemnt


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures:
# One character vector with all unique values

a <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
a

# One character vector with exactly 3 unique values
b <- c('w','w','w','w','w','y','y','y','y','y','n','n','n','n','n')
b
# One numeric vector with all unique values
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
c
# One numeric vector with some repeated values (number of your choosing)
d <- c(11,11,11,11,11,5,5,5,5,5,20,20,20,20,20)

d
# One numeric vector with some decimal values (of your choosing)
e <- c(1.1,1.2,1.3,1.4,1.5,2.1,2.2,2.3,2.4,2.5,3.1,3.2,3.3,3.4,3.5)
e


# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.
data <- cbind(a,b,c,d,e,f)
data

df <- as.data.frame(data)
df

colnames(df) <- c('Salmon','King','Pink','Chum','Coho','Sockeye')
df

row.names(df) <- df$salmon
df

# Remove the character vector with unique values from the data frame.

df[,-1]

# Add 1 row with unique numeric values to the data frame.

f <- c(42,43,56,78,98,87,76,9,56,45,57,32,34,24,42)
f

# Export the data frame as a .csv file

df <- data.frame ('King' = c('w','w','w','w','w','y','y','y','y','y','n','n','n','n','n'),'Pink' = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),'Chum' = c(11,11,11,11,11,5,5,5,5,5,20,20,20,20,20),'Coho' = c(1.1,1.2,1.3,1.4,1.5,2.1,2.2,2.3,2.4,2.5,3.1,3.2,3.3,3.4,3.5),'Sockeye' = c(42,43,56,78,98,87,76,9,56,45,57,32,34,24,42))
print (df)

write.csv(df,"C:\\Users\\Tryon\\Desktop\\Test\\People.csv"row.Salmon = FALSE)

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading.

summary(df)

#Summary ####
#Salmon              King               Pink               Chum               Coho             Sockeye        
#Length:15          Length:15          Length:15          Length:15          Length:15          Length:15        
#Class :character   Class :character   Class :character   Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  

# Push your script and your .csv file to GitHub in a new "Week1" folder.
