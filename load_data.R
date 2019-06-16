### Load in Whatsapp data
chat <- read.table("C:/Users/angus/Dropbox/whatsapp chats/family.txt" , 
                   fill=TRUE, quote="", sep = "\n", encoding = "UTF-8" )
names(chat) <-"Raw"


### Need to subset the string into different variables 
#Time, Date, Sender and message 
library(tidyverse)

chat$Raw <- gsub("\\]", " ", chat$Raw )
reg <- "(\\d+/\\d+/\\d+),\\s+(\\d+:\\d+:\\d+)\\s+(\\w+):\\s+(.*)"
temp <- chat %>% 
  tidyr::extract(Raw, c("date", "time", "sender", "message"),reg , remove = FALSE)
temp <- filter(temp, !is.na(sender))
temp <- temp[-c(1),] ##get rid of the "FAMILY" sender



###dataset for each individual, then appending them all together
temp$sender <-factor(temp$sender)
i = 0
for (n in levels(temp$sender)) {
i <- i + 1
sender_n = filter(temp, temp$sender == n)
freq <- as.data.frame(table(sender_n$date, sender_n$sender))
names(freq) <-c("Date","Sender", "Freq")
freq <- freq %>%
  mutate(Date = as.Date(Date, format="%d/%m/%Y")) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"), fill = list(Freq = 0, Sender = n)) 
if (i>1) {
 master<- rbind.data.frame(freq, master)
}
else {
 master <- freq
}
}


## Frequency line graph ##

ts(master$Freq, frequency = 365)
ggplot(data = master, aes(x = Date, y = Freq, colour=Sender)) +
  geom_line() + theme(panel.background = element_blank())