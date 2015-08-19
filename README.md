# gScholarAnalysis

# gScholarAnalysis

I saw [this post] (http://tuxette.nathalievilla.org/?p=1682) a few weeks ago on [R-bloggers] (http://www.r-bloggers.com/). It was an interesting idea of pulling down your Google Scholar data, processing it some and extracting interesting bits about it. The post that inspired this person is [here] (http://rogiersbart.blogspot.fr/2015/05/put-google-scholar-citations-on-your.html), wherein he pulled down his data and made a simple plot showing how many publications he had per year since the first one. It's an interesting proof-of-concept, but this plot already prominently sites on your Google Scholar profile page. What interested me was the co-author index and the word cloud from your publications' titles.

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



