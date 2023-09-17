library(tidyverse)
data(english)

data(english)
d = english |> 
  select(RTlexdec, Familiarity, Word, AgeSubject) |> 
  as_tibble()

d
