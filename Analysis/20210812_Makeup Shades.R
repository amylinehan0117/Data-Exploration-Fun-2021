#############################################################################
# 8/12/2021
#
# Source: https://github.com/the-pudding/data/tree/master/makeup-shades 
#
#
#
#
#
#############################################################################

# I. Set-Up ---------------------------------------------------

rm(list = ls())
gc(reset=T)

library('tidyverse')
library('ggplot2')

# I. Load Data -----------------------------------------------

dta <- read.csv(file = './Data/shades.csv', stringsAsFactors = F)

head(dta)

# Field info
# brand = brand that created foundation
# brand_short = abbr of brand name  (unique)
# product = full name for liquid foundation and/or product w the highest number of shades if brand has multipel
# product_short = unique product 
# hex = hexacdecimal code for a particular shade (w/out leading #)
#  H = hue value from the HSBC/HSV color space; represented as a number from 0 -360 around the color wheel
# S = saturation value (0-360)
# V = value/brightness' describes the brightness/intensity of a color from 0 -100 (represented via 0-1)
# L = lightness value that describes the brightness/intensity o a color from  0-100 (represented via 0-1)
#group= group with which this product should be analyzied 

# 7 groups in this data: 
# 0: Fenty Beauty's PRO FILT'R Foundation Only
# 1: Make Up For Ever's Ultra HD Foundation Only
# 2: US Best Sellers
# 3: BIPOC-recommended Brands with BIPOC Founders
# 4: BIPOC-recommended Brands with White Founders
# 5: Nigerian Best Sellers
# 6: Japanese Best Sellers
# 7: Indian Best Sellers

# II. Initial Exploration ----------------------------------

head(dta)

# Shade Counts

ct <- dta %>% 
        mutate(name = paste0(brand, ' : ', product)) %>% 
        group_by(name) %>% 
        summarise( ct = n()) %>%
        #filter(ct> 20) %>% 
        arrange(-ct ) 

plot1 <- ggplot(ct  , aes(x = ct, y = name ))


plot1 + geom_bar(stat = 'identity', fill='pink') + 
  xlab('Brand and Product') + 
  ylab('Number of Shades') +
  ggtitle( "Review of Shade Range for US Best Selling Foundation") +
  theme_bw()+
  scale_y_discrete(limits=rev, expand = c(0,0))+
  scale_x_continuous( expand = c(0, 0)) 


dta %>% group_by(hex) %>% summarise(ct = n()) %>% filter(ct >1) %>% arrange(-ct)


dta %>% filter(hex=='8c5b3d')

# Colors 

col <-dta %>%  mutate(name = paste0(brand, ' : ', product)) %>% 
              mutate(col = paste0('#', hex))

head(col)


plot2 <- ggplot(col , aes(x = name, fill = col)) 

plot2 + geom_bar(position='stack', stat='count')


# Explore fun package:
#install.packages('esquisse')
#library(esquisse)
#esquisser()



