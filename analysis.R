library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)


dim(pet_sales)
length(levels(as.factor(pet_sales$product_id)))
length(levels(as.factor(pet_sales$product_category)))
pet_sales$sales <- as.numeric(str_remove_all(pet_sales$sales, '\\$|,'))
class(pet_sales$price)
length(levels(as.factor(pet_sales$vendor_id)))
length(levels(as.factor(pet_sales$pet_size)))
length(levels(as.factor(pet_sales$pet_type)))
levels(as.factor(pet_sales2$pet_type))

pet_sales %>% filter(pet_type %in% c('cat', 'dog', 'fish', 'bird')) -> pet_sales
pet_sales %>% mutate(re_buy = as.factor(re_buy)) -> pet_sales

length(levels(as.factor(pet_sales$rating)))
length(levels(as.factor(pet_sales$re_buy)))

ggplot(pet_sales, aes(re_buy)) + geom_bar() + ggtitle("products purchased more than once")
table(pet_sales$re_buy)

ggplot(pet_sales, aes(re_buy, sales)) + geom_boxplot() + ggtitle("sales")
ggplot(pet_sales, aes(re_buy, price)) + geom_boxplot() + ggtitle("price")

t.test(subset(pet_sales, re_buy == '1')$price, subset(pet_sales, re_buy == '0')$price)
t.test(subset(pet_sales, re_buy == '1')$sales, subset(pet_sales, re_buy == '0')$sales)

wilcox.test(price ~ re_buy,data=pet_sales)
wilcox.test(sales ~ re_buy,data=pet_sales)

bartlett.test(price ~ re_buy,data=pet_sales)
bartlett.test(sales ~ re_buy,data=pet_sales)

pet_sales %>% group_by(re_buy) %>% summarise(mean = mean(sales), sd=sd(sales))
pet_sales %>% group_by(re_buy) %>% summarise(mean = mean(price), sd=sd(price))

ggplot(pet_sales, aes(sales)) + stat_bin(bins = 15) + ggtitle('Distribution of sales')

pet_sales_r1 <- subset(pet_sales, re_buy == '1')

ggplot(pet_sales_r1, aes(fct_rev(fct_infreq(product_category)))) +
  geom_bar() + 
  facet_wrap(~pet_type) +
  coord_flip() +
  xlab('product_category') +
  ggtitle('re-bought products by category')

ggplot(pet_sales, aes(fill = re_buy, fct_rev(fct_infreq(product_category)))) +
  geom_bar() + 
  facet_wrap(~pet_type) +
  coord_flip() +
  xlab('product_category') +
  ggtitle('products by category')

cut(pet_sales$price, breaks = 5)

pet_sales$price_level <- recode(cut(pet_sales$price, breaks = 5),
       '(0.0977,50.8]' = 'very cheap',
       '(50.8,101]' = 'cheap',
       '(101,152]' = 'medium',
       '(152,202]' = 'expensive',
       '(202,253]' = 'very expensive')

ggplot(subset(pet_sales, re_buy == '1'), aes(price_level)) +
  geom_bar() + 
  facet_wrap(~pet_type) +
  coord_flip() +
  xlab('price level') +
  ggtitle('re-bought products by price level')



ggplot(subset(pet_sales, re_buy == '1'), aes(as.factor(rating))) +
  geom_bar() + 
  facet_wrap(~pet_type) +
  coord_flip() +
  xlab('rating') +
  ggtitle('re-bought products by rating')

ggplot(pet_sales, aes(fill = re_buy, as.factor(rating))) +
  geom_bar() + 
  facet_wrap(~pet_type) +
  coord_flip() +
  xlab('rating') +
  ggtitle('re-bought products by rating')

pet_sales %>% filter(pet_type == "dog",
                     product_category == "Equipment",
                     re_buy == '1',
                     price_level %in% c('expensive', 'very expensive'),
                     rating == '10') %>% 
  dim()
13

pet_sales %>% filter(pet_type == "cat",
                     product_category == "Equipment",
                     re_buy == '1',
                     price_level %in% c('expensive', 'very expensive')) %>% 
  dim()
10

pet_sales %>% filter(pet_type == "bird",
                     product_category == "Equipment",
                     re_buy == '1',
                     price_level %in% c('expensive', 'very expensive')) %>% 
  dim()
3

pet_sales %>% filter(pet_type == "fish",
                     product_category == "Equipment",
                     re_buy == '1',
                     price_level %in% c('expensive', 'very expensive')) %>% 
  dim()
6

