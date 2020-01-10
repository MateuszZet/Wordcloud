
rquery.wordcloud <- function(x, lang = "english", colorPalette = "Dark2", min.freq=10, max.words = 100) {

	#all necessery libriaries
	library("tm")
	library("wordcloud")
	library("RColorBrewer")

	#read from file
	text <- readLines(x)

	# Load the text as a corpus
	docs <- Corpus(VectorSource(text))
	
	# Convert the text to lower case
	docs <- tm_map(docs, content_transformer(tolower))
	
	# Remove numbers
	docs <- tm_map(docs, removeNumbers)
	
	# Remove stopwords for the language 
	docs <- tm_map(docs, removeWords, stopwords(lang))
	
	# Remove punctuations
	docs <- tm_map(docs, removePunctuation)
	
	# Eliminate extra white spaces
	docs <- tm_map(docs, stripWhitespace)
	
	# Create term-document matrix
	tdm <- TermDocumentMatrix(docs)
	m <- as.matrix(tdm)
	v <- sort(rowSums(m), decreasing = TRUE)
	d <- data.frame(word = names(v), freq = v)

	# RColorBrewery documentation: https://www.rdocumentation.org/packages/RColorBrewer/versions/1.1-2/topics/RColorBrewer 
	# check the color palette name 
	if (!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
	else colors = brewer.pal(8, colorPalette)

	# Plot the word cloud
	set.seed(1234)
	# WordCloud documentation: https://www.rdocumentation.org/packages/wordcloud/versions/2.6/topics/wordcloud
	wordcloud(d$word, d$freq, min.freq = min.freq, max.words = max.words,
			random.order = FALSE, rot.per = 0.35,
			use.r.layout = FALSE, colors = colors)

	invisible(list(tdm = tdm, freqTable = d))
}
