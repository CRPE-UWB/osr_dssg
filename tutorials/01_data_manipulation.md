# Data Manipulation
## Reading in data tables

You have several option to load data in R (depending on the type of data). For comma seperated values you can use:
- `read.csv()` *base function*
- `read_csv()` *dplyr function*

Refer to the functions documentation.  You can use `?read.csv` or `help(read.csv)` to look at documentation.

For example we can load our example data, `inflammation-01.csv`, which looks like this:
```
0,0,1,3,1,2,4,7,8,3,3,3,10,5,7,4,7,7,12,18,6,13,11,11,7,7,4,6,8,8,4,4,5,7,3,4,2,3,0,0
0,1,2,1,2,1,3,2,2,6,10,11,5,9,4,4,7,16,8,6,18,4,12,5,12,7,11,5,11,3,3,5,4,4,5,5,1,1,0,1
0,1,1,3,3,2,6,2,5,9,5,7,4,5,4,15,5,11,9,10,19,14,12,17,7,12,11,7,4,2,10,5,4,2,2,3,2,2,1,1
0,0,2,0,4,2,2,1,6,7,10,7,9,13,8,8,15,10,10,7,17,4,4,7,6,15,6,4,9,11,3,5,6,3,3,4,2,3,2,1
0,1,1,3,3,1,3,5,2,4,4,7,6,5,3,10,8,10,6,17,9,14,9,7,13,9,12,6,7,7,9,6,3,2,2,4,2,0,1,1
``` 
Just comma separated values with no headers.
To load we need to know where that file lives. My working directory is this projects repo, and the file lives in the `tutorials` directory.
```r
read.csv(file = "tutorials/inflammation-01.csv", header = FALSE)
```
You can assign the data table to an object, which will help with manipulation. 
```r
df1 <- read.csv(file = "tutorials/inflammation-01.csv", header = FALSE)
```
**Headers**
In this case out file doesn't have headers, but in our data we will have headers.  The default for `read.csv` is `headers = TRUE` and you don't need to specify defaults.
```r
df1 <- read.csv(file = "tutorials/inflammation-01.csv")
```
## Manipulating Data
When you assign a data table to an object, you can view its contetns in a variaety of ways. For large data sets it is convenient to use the function `head` to display only the first few rows of data.

```r
head(df1)
```
You can look at the class of the object using `class()`. You can look at the dimensions using `dim()`.
```r
class(df1)
dim(df1)
```
If we want to get a single value from the data frame, we can provide an index in square brackets. The first number specifies the row and the second the column:
```r
df1[1,1]
```
If we want to select more than one row or column, we can use the function c, which stands for combine. For example, to pick columns 10 and 20 from rows 1, 3, and 5, we can do this:
```r
df1[c(1, 3, 5), c(10, 20)]
```
We frequently want to select contiguous rows or columns, such as the first ten rows, or columns 3 through 7. You can use c for this, but it’s more convenient to use the : operator. This special function generates sequences of numbers:
```r
1:5
```
We can use this to subset:
```r
df1[1:4, 1:10]
```
If you want to select all rows or all columns, leave that index value empty.
```r
# All columns from row 5
df1[5, ]

# All rows from column 16-18
df1[, 16:18]
```
### Addressing columns by name.
Columns can also be addressed by name, with either the `$` operator (ie. `df1$V16`) or square brackets (ie. `df1[, 'V16']`). You can learn more about sub-setting by column name in this supplementary lesson.

## Built in summary functions

You can use `summary()` on the whole data frame or individual column.
```r
summary(df1)
#or
summary(df1[,2])
```
using `%>%` and the `Tidyverse`
```r
install.packages('tidyverse')
library(tidyverse)
```
The tidyverse is a collection of R packages that share common principles and are designed to work together seamlessly.  Great for data manipulation and visualization.

## Useful functions for data exploration

- `glimpse()` *information dense summary of table*
- `group_by()` *group data by column values*
- `filter()` *filter by one or more column values, takes boolean conditions (`&`, `|`, `==`, etc.)
- `select()` *select specific columns*
- `mutate()` *create new column (takes other column values or conditions as inputs)*
- `summarise()` *summarise aggregated data, best used with a `group_by()` statement*

Using pipes `%>%`, they help you chain functions on a specific data frame.

Select first 10 columns:
```r
df1 %>%
select(V1:V10)
```
Select the first 5 columns and filter by the value  `1` on a specific column.
```r
df1 %>%
select(V1:V5) %>%
filter(V2 ==1)
```
Find missing values:
You can use `is.na` and it gives you boolean `TRUE` or `FALSE`
```r
is.na(df1)
```
Summarise missing values for all columns.
```r
missing_values <- df1 %>% 
  summarize_all(funs(sum(is.na(.))/n()))
```

### You can visualize
```r
missing_values <- gather(missing_values, key="feature", value="missing_pct")

missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()
```
