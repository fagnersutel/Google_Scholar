
library(scholar)
# ID Wolnei Google cademico
id <- 'ieK801EAAAAJ'
l <- get_profile(id)
l$name
l$affiliation
l$h_index
p <- get_publications(id)
dim(p)
pubs = get_publications(id)
plot(p$cite, main="Citacoes em Artigos", sub="->",
     xlab="Artigos", ylab="Numero de Citacoes",)

head(pubs)
head(p, 3)
ct <- get_citation_history(id)
predict_h_index(id)
library(ggplot2)
ggplot(ct, aes(year, cites)) + geom_line() + geom_point()
as.character(p$title[1])
ach <- get_article_cite_history(id, p$pubid[1])
ggplot(ach, aes(year, cites)) +
  geom_segment(aes(xend = year, yend = 0), size=1, color='darkgrey') +
  geom_point(size=3, color='firebrick')
#Volnei x Iraci Toprres 
ids <- c(id, 'HqmB9qkAAAAJ')
cs <- compare_scholars(ids)
cs <- subset(cs, !is.na(year) & year > 1900)
ggplot(cs, aes(year, cites, group=name, color=name)) + geom_line() + theme(legend.position="bottom")


csc <- compare_scholar_careers(ids)
ggplot(csc, aes(career_year, cites, group=name, color=name)) + geom_line() + geom_point() +
  theme(legend.position=c(.2, .8))



coauthor_network <- get_coauthors(id, n_coauthors = 10)
plot_coauthors(coauthor_network)


#https://rstudio-pubs-static.s3.amazonaws.com/289471_47981df21396472c99c2495339da39d1.html
#https://www.r-bloggers.com/yet-another-post-on-google-scholar-data-analysis/
#https://datascienceplus.com/google-scholar-scraping-with-rvest/


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

get_abstract = function(pub_id, my_id) {
  print(pub_id)
  paper_url = paste0("http://scholar.google.com/citations?view_op=view_citation&hl=fr&user=",
                     my_id, "&citation_for_view=", my_id,":", pub_id)
  paper_page = htmlTreeParse(paper_url, useInternalNodes=TRUE, encoding="utf-8")
  paper_abstract = xpathSApply(paper_page, "//div[@id='gsc_descr']", xmlValue)
  return(paper_abstract)
}
get_all_abstracts = function(id) {
  all_publications = get_all_publications(my_id)
  all_abstracts = sapply(all_publications$pubid, get_abstract)
  return(all_abstracts)
}
  
get_all_abstracts(id)

library(XML)
all_abstracts = get_all_abstracts(id)
all_abstracts


library(tm)
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
library(wordcloud)
wordcloud(names(important), important, random.color=TRUE, random.order=TRUE,
          color=brewer.pal(12, "Set3"), min.freq=1, max.words=length(important), scale=c(3, 0.3))
