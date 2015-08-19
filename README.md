# gScholarAnalysis

# gScholarAnalysis

I saw [this post] (http://tuxette.nathalievilla.org/?p=1682) a few weeks ago on [R-bloggers] (http://www.r-bloggers.com/). It was an interesting idea of pulling down your Google Scholar data, processing it some and extracting interesting bits about it. The post that inspired this person is [here] (http://rogiersbart.blogspot.fr/2015/05/put-google-scholar-citations-on-your.html), wherein he pulled down his data and made a simple plot showing how many publications he had per year since the first one. It's an interesting proof-of-concept, but this plot already prominently sites on your Google Scholar profile page. What interested me was the co-author index and the word cloud from your publications' titles.

The problem was that the post had a few typoes. :/ 

# The get_all_publications() function pulls down the author of interests' publication list for further processing using get_publications() from the scholar package. Load the function by copying and pasting into your R session. It gets around the 20 publications per page limit on Google Scholar by looping through them 20 at a time.
```R
get_all_publications = function(authorid) {
  # initializing the publication list
  all_publications = NULL
  # initializing a counter for the citations
  cstart = 0
  # initializing a boolean that check if the loop should continue
  notstop = TRUE
 
  while (notstop) {
    new_publications = try(get_publications(my_id, cstart=cstart), silent=TRUE)
    if (class(new_publications)=="try-error") {
      notstop = FALSE
    } else {
      # append publication list
      all_publications = rbind(all_publications, new_publications)
      cstart=cstart+20
    }
  }
  return(all_publications)
}
```
The get_all_coauthors() function pulls down your publications, isolates the author of interests' coauthors using one of two methods. If you do not define "me", it assumes the author's profile name is FirstName LastName and takes the second name as unique within each publications author list. It then eliminates that authors name from the list of co-authors using a simple grep search. You can define me using the author's last name or first name as long as it is unique within your list of co-authors.
```R
get_all_coauthors = function(my_id, me=NULL) {
  all_publications = get_all_publications(my_id)
  if (is.null(me))
    me = strsplit(get_profile(my_id)$name, " ")[[1]][2]
  # make the author list a character vector
  all_authors = sapply(all_publications$author, as.character)
  # split it over ", "
  all_authors = unlist(sapply(all_authors, strsplit, ", "))
  names(all_authors) = NULL
  # remove "..." and yourself
  all_authors = all_authors[!(all_authors %in% c("..."))]
  all_authors = all_authors[-grep(me, all_authors)]
  # make a data frame with authors by decreasing number of appearance
  all_authors = data.frame(name=factor(all_authors, 
    levels=names(sort(table(all_authors),decreasing=TRUE))))
}
```
The get_all_abstracts() function pulls down the abstracts for word cloud analysis using the get_abstract() function. 
``` R
get_all_abstracts = function(my_id) {
  all_publications = get_all_publications(my_id)
  all_abstracts = sapply(all_publications$pubid, get_abstract)
  return(all_abstracts)
}
get_abstract = function(pub_id, my_id) {
  print(pub_id)
  paper_url = paste0("http://scholar.google.com/citations?view_op=view_citation&hl=fr&user=",
                     my_id, "&citation_for_view=", my_id,":", pub_id)
  paper_page = htmlTreeParse(paper_url, useInternalNodes=TRUE, encoding="utf-8")
  paper_abstract = xpathSApply(paper_page, "//div[@id='gsc_descr']", xmlValue)
  return(paper_abstract)
}
```


```R
#load required packages
require(scholar)
require(ggplot2)
require(dplyr)
require(tm) #for word processing
require(wordcloud)
require(XML)


my_id = "6jGizt0AAAAJ" #substitute your own ID to see yours

all_authors = get_all_coauthors(my_id, me="Hurr") # replace your name with mine to make sure it eliminates you

main_authors = all_authors[all_authors$name %in% names(which(table(all_authors$name)>1))]

ggplot(main_authors, aes(x=name) + geom_bar(fill=brewer.pal(3, "Set2")[2]) +
  xlab("co-author") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))

all_abstracts = get_all_abstracts(my_id)

# transform the abstracts into "plan text documents"
all_abstracts = lapply(all_abstracts, PlainTextDocument)

# find term frequencies within each abstract
terms_freq = lapply(all_abstracts, termFreq, 
                    control=list(removePunctuation=TRUE, stopwords=TRUE, removeNumbers=TRUE))

# finally obtain the abstract/term frequency matrix
all_words = unique(unlist(lapply(terms_freq, names)))
matrix_terms_freq = lapply(terms_freq, function(astring) {
  res = rep(0, length(all_words))
  res[match(names(astring), all_words)] = astring
  return(res)
})
matrix_terms_freq = Reduce("rbind", matrix_terms_freq)
colnames(matrix_terms_freq) = all_words

# deduce the term frequencies
words_freq = apply(matrix_terms_freq, 2, sum)

# keep only the most frequent and after a bit of cleaning up (not shown) make the word cloud
important = words_freq[words_freq > 10]

wordcloud(names(important), important, random.color=TRUE, random.order=TRUE,
          color=brewer.pal(12, "Set3"), min.freq=1, max.words=length(important), scale=c(3, 0.3))
``` 



