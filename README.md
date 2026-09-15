# Watershed Analysis
## [LINK TO THE PROJECT](https://avrillozano.shinyapps.io/Project3/)
<img width="2962" height="1936" alt="AustinSkylineLouNeffPoint-Jun2010-a" src="https://github.com/user-attachments/assets/3c892b9f-88af-46ab-84f8-b6b1631cdb77" />

## Description
The other day, I was on a run across Shoal Creek leading into Lady Bird lake. As I was running, I was noticing that the water was very disgusting, full of scum and pipe runoff. I wondered to myself, *what even is in this water?* *Has it changed a lot over time?* I did some research and found 
a database of water quality testing done by the City of Austin from 1982 to 2025, which piqued my interest. By sifting through thousands of water samples, I set out to answer a simple question: What kinds of metals are in the local water, and how are they changing over time?
## My takeaways
1. Most levels are very low. More than 85% of all samples showed very small amounts of metals (under 100 micrograms per liter).
2. Metal levels don't steadily go up or down over the years. Instead, they spike randomly (probably during big weather events like big rainstorms (analysis on that coming soon!)) and quickly settle back down.
3. Lady Bird Lake generally has slightly higher average metal levels than Shoal Creek. This makes sense because Shoal Creek flows right into Lady Bird Lake, bringing runoff along with it.
Everyday / Essential Metals: Metals like Aluminum, Iron, Zinc, Copper, and Calcium. These occur naturally in rocks, soil, and human infrastructure.
## General results
Within non-toxic metals that occur naturally, like Aluminum, Iron, Zinc, Copper, and Calcium, the highest overall were Aluminum and Strontium.

Within the toxic metals, Lead and Cadmium made up the majority of the tests. Additionally, Lead and Barium showed the highest average readings among toxic metals, though overall numbers stayed low.

## Packages used
```{r}
library(shiny)
library(bslib)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(ggridges)
library(lubridate)
```
## Sources
Water Quality Sampling Data | Open Data | City of Austin, Texas. (Accessed 2025, November 13). https://data.austintexas.gov/Environment/Water-Quality-Sampling-Data/5tye-7ray/about_data

Wikimedia Commons. (n.d.). Austin Skyline. https://upload.wikimedia.org/wikipedia/commons/0/09/AustinSkylineLouNeffPoint-Jun2010-a.JPG
