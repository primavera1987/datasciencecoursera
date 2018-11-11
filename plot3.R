## Load the library
library(data.table)

## Read and tidy data
df = fread('household_power_consumption.txt')
df[df == '?'] = NA

df$Date = as.Date(df$Date, format = "%d/%m/%Y")
df = df[df$Date == as.Date('2007-02-01') | df$Date == as.Date('2007-02-02')]

# Get date time from Date and Time fields
DateTime = strptime(paste(df$Date, df$Time), '%Y-%M-%d %H:%M:%S')

df$Sub_metering_1 = as.numeric(df$Sub_metering_1)
df$Sub_metering_2 = as.numeric(df$Sub_metering_2)
df$Sub_metering_3 = as.numeric(df$Sub_metering_3)

## Plot and save to file
png(file = "plot3.png", width = 480, height = 480, units = "px")

plot(DateTime, df$Sub_metering_1, 
     type = 'l', 
     ylab = 'Energy sub metering', 
     xlab = '')
points(DateTime, df$Sub_metering_2, type = 'l', col = 'red')
points(DateTime, df$Sub_metering_3, type = 'l', col = 'blue')
legend('topright', 
       col = c('black', 'red', 'blue'), 
       legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
       lty = 1)
dev.off()