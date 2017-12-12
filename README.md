# R-ETS

This is a R script created to scrape data from the ETS account holders on the european registry.

It uses some common libraries for R, such Dplyr, Stringr, XML, and above all the great Rvest.

The website is quite complicated to scrape, due to :

- The strange URL pattern
- The ugly design of the webpage
- ...

Basically, I use two parts in the script.

- 1°) The first part scrapes the 82x pages table of account holders, and collects the URL of the detailled view. As this URL is pretty long, the output csv file is pretty big.

- 2°) The second part explores each of these URL and scrapes the data.

At the end the R script merges everything as a single dataset, smaller than the 1st one.

One of the tricky part is to transform the table of allowance in a single line of data, using the reshape2 library, without losing the title of the columns.

This script can of course be optimized by using functions (I only use two of them in the first part).

I've intented to be explanatory, so the code has a lot of comments.
