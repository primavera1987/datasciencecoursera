## Load the library
library(data.table)

## Read and tidy data
df = fread('household_power_consumption.txt')
df[df == '?'] = NA

df$Date = as.Date(df$Date, format = "%d/%m/%Y")
df = df[df$Date == as.Date('2007-02-01') | df$Date == as.Date('2007-02-02')]

df$Global_active_power = as.numeric(df$Global_active_power)

# Get date time from Date and Time fields
DateTime = strptime(paste(df$Date, df$Time), '%Y-%M-%d %H:%M:%S')

## Plot and save to file
png(file = "plot2.png", width = 480, height = 480, units = "px")
plot(DateTime, df$Global_active_power, 
     type = 'l', 
     ylab = 'Global Active Power (kilowatts)', 
     xlab = '')
dev.off()
