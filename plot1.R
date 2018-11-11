## Load the library
library(data.table)

## Read and tidy data
df = fread('household_power_consumption.txt')
df[df == '?'] = NA

df$Date = as.Date(df$Date, format = "%d/%m/%Y")
df = df[df$Date == as.Date('2007-02-01') | df$Date == as.Date('2007-02-02')]

df$Global_active_power = as.numeric(df$Global_active_power)

## Plot the histogram and save to file
png(file = "plot1.png", width = 480, height = 480, units = "px")
hist(df$Global_active_power, col = 'red', main = 'Global Active Power', 
     xlab = 'Global Active Power (kilowatts)')
dev.off()