# NLP-twitter-science-news
Using natural language processing on tweets from selected users to identify what science stories are most popular

### This Repo is under construction

## Summary
The overarching goal of this project is to gain a deeper, more quantitative understanding of how science news stories are received. This stems from the notoriously difficult task of reporting on scientific findings. Balancing 
generating excitement, managing expectations, and including important but mundane details is challenging. Twitter provides
a simple paradigm for studying what kinds of stories are reported and how they are preceived by consumers.

## Bite sized questions:

### Which tweets?
There are a lot of tweets about science, so to make the search a little easier I selected some users to follow that tweet 
about science news. Here's the list:

Scientific journals (based on impact factor):
Nature (@nature)
Science (@sciencemagazine)
Cell (@CellCellPress)

Medical journals (based on impact factor):
New England Journal of Medicine (@NEJM)
Journal of the American Medical Association (@JAMA_current)
The Lancet (@TheLancet)

(US) Government funded organizations: 
NASA (@NASA)
NIH (@NIH)
The Smithsonian (@smithsonian)

Science branch of popular news outlets:
NYT Science Times (@NYTScience)
NPR Science Desk (@nprscience)
Scientific American (@sciam)
WIRED (@WIRED)

Reputable scientists active on twitter:
Neil DeGrasse Tyson (@neiltyson)
Ben Goldacre (@bengoldacre)

### Which stories are most popular?
An easy first step is figureing out which stories are most popular amoung twitter users. Figuring this out can help set the
stage for gaining a deeper understanding of how science stories are interpreted. Each tweet has metadata that includes 
retweetCount and favoriteCount. Together these can be used as a proxy for the popularity of a science story.

#### How to characterize the most popular science stories?
Ideally popular science stories will have some kind of trend that separates them from unpopular ones. For example, they can
be about interesting topics or from a user everyone likes. To get at this the "tm" package will be used to first clean up the
text of each tweet and then pick up which words are most common and what other they are associated with. Other packages,
such as "cluster", can be used to figure out how the over all text of tweets cluster.

More to come...

