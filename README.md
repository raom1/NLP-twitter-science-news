# NLP-twitter-science-news
Using natural language processing on tweets from selected users to identify what science stories are most popular

## Summary
The overarching goal of this project is to gain a deeper, more quantitative understanding of how science news stories are
received. This stems from the notoriously difficult task of reporting on scientific findings. Balancing generating excitement,
managing expectations, and including important but mundane details is challenging. Twitter provides a simple paradigm for
studying what kinds of stories are reported and how they are preceived by consumers. An intial analysis of what differentiates
popular versus unpopular tweets about science news can help to identify particular topics that are of greater interest to
twitter users. The results of this analysis are available as a shiny app available <a href="https://maheshrao.shinyapps.io/science_tweets_nlp/" target="_blank">here</a>.

## Bite sized questions:

### Which tweets?
There are a lot of tweets about science, so to make the search a little easier I selected some users to follow that tweet 
about science news. Here's the list:

Scientific journals (based on impact factor): <br>
Nature (@nature)<br>
Science (@sciencemagazine)<br>
Cell (@CellCellPress)<br>
<br>
Medical journals (based on impact factor):<br>
New England Journal of Medicine (@NEJM)<br>
Journal of the American Medical Association (@JAMA_current)<br>
The Lancet (@TheLancet)<br>
<br>
(US) Government funded organizations: <br>
NASA (@NASA)<br>
NIH (@NIH)<br>
The Smithsonian (@smithsonian)<br>
<br>
Science branch of popular news outlets:<br>
NYT Science Times (@NYTScience)<br>
NPR Science Desk (@nprscience)<br>
Scientific American (@sciam)<br>
WIRED (@WIRED)<br>
<br>
Reputable scientists active on twitter:<br>
Neil DeGrasse Tyson (@neiltyson)<br>
Ben Goldacre (@bengoldacre)<br>

### Which tweets to study?
In order to study only original tweets, retweets were excluded from analysis

### Which stories are most popular?
An easy first step is figureing out which stories are most popular amoung twitter users. Figuring this out can help set the
stage for gaining a deeper understanding of how science stories are interpreted. Each tweet has metadata that includes 
retweetCount and favoriteCount. Together these can be used as a proxy for the popularity of a science story. To calculate
popularity the retweetCount and favoriteCount were added together and then divided by the total number of followers for the
posting user. This is to account for the possibility that users may have different numbers of followers, presumably affecting
how many people would see the tweet. 100 retweets and likes from a user with 1000 followers would mean something different
than a user with 10000 followers. There is no clear separation between popular and unpopular tweets so the top 10% of tweets
were taken as popular and the others were unpopular.

### How to study tweets?
To study the contents of tweets and identify what differentiates them they need to be cleaned. To do this special characters,
stopwords, links, hashtags, and mentions were removed. To tokenize words in each tweet a dataframe was constructed where each
row was one word and accompanying information for user, date, and id.

### How to characterize the popular and unpopular tweets about science stories?
Ideally popular science stories will have some kind of trend that separates them from unpopular ones. For example, they can
be about interesting topics or from a user everyone likes. To address this three visualizations were prepared to help identify
differences between popular and unpopular tweets. 

#### Wordclouds
Wordclouds were created for popular and unpopular tweets. The wordclouds were based on the frequency of words in each category
with larger words representing greater frequency. By comparing the most frequent words in each category trends can be observed
as to topics that are more prevalent in popular or unpopular tweets.

#### Sentiment Analysis
Sentiment analysis was performed and the most frequent words in each group and their associated positive or negative sentiment
were displayed in a bar chart. Comparisons can then be made to see if a particular group is more often positive or negative.

#### Word Correlation
Looking at individual words doesn't always give the complete picture of the content of each tweet. To help address this word
correlations were made for each word pair in each group. Correlations were based on how often two words appered next to each
other versus how often each word appeared independently. A visualization was then prepared to display correlations and
represented groups of related words that could be placed into topics. Popular and unpopular tweets could then be compared to
identify stories or topics that were more common in popular versus unpopular tweets.

### How to interact with the data?
Looking at all tweets can be useful but sometimes it is better to look at tweets from certain users or over certain time
frames can be useful. To address this tweets can be subdivided based on the week they were collected or based on a list of
selected users. All of this is available through a shiny app found <a href="https://maheshrao.shinyapps.io/science_tweets_nlp/" target="_blank">here</a>

## Future Directions
The analyses shown here only scratch the surface of what is possible and what can be done to help improve science communication. Future work will include topic identification using various methods such as clustering, twitter searches based on specific topic criteria, and further analyses of the resulting tweets to try to uncover possible divergent or misunderstandings about topics.
