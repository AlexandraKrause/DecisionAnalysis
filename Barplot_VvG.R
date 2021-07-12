library(tidyverse)
library(readxl)

test<-read_excel("./Test.xlsx") %>%
  gather(key = "key", value = "value")

test %>% ggplot(aes(x = key, y = value)) +
  geom_bar(stat = "identity")

