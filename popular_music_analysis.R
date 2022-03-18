library(ggplot2)
library(ggpubr)
library(plyr)
library(dplyr)
library(hrbrthemes)
library(devtools)
library(ggbiplot)
library(syuzhet)
library(spacyr)
library(stringr)
library(textstem)
library(viridis)
library(tm)
library(stopwords)
library(corrplot)
library(psych)
library(quanteda)
library(quanteda.textplots)
library(scales)
library(topicmodels)
library(SnowballC)
library(tidytext)
library(tidyr)

####Import dataset
music_df <- read.csv("music_df.csv")
summary(music_df)
describeBy(music_df, group=music_df$year_bin, fast=TRUE)

##################Musical features analysis##################

#####Box plots by year bin and gender 
ordered_year_bin <- factor(music_df$year_bin, levels=c("50s", "60s", "70s", "80s", "90s", "00s", "10s"))

ggplot(music_df, aes(fill=Gender, y=danceability, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=energy, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=loudness, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=speechiness, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=acousticness, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=instrumentalness, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=liveness, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=valence, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")

ggplot(music_df, aes(fill=Gender, y=tempo, x=ordered_year_bin)) + 
  geom_bar(position="stack", stat="identity")


#####Violin plots by year bin
ggplot(music_df, aes(x=ordered_year_bin, y=danceability)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

ggplot(music_df, aes(x=ordered_year_bin, y=energy)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

ggplot(music_df, aes(x=year_bin, y=loudness)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

ggplot(music_df, aes(x=ordered_year_bin, y=speechiness)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

ggplot(music_df, aes(x=ordered_year_bin, y=acousticness)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

#Instrumentalness values too small

ggplot(music_df, aes(x=ordered_year_bin, y=liveness)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

ggplot(music_df, aes(x=ordered_year_bin, y=valence)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)

ggplot(music_df, aes(x=ordered_year_bin, y=tempo)) + 
  geom_boxplot(width=0.2,fill="gold") + 
  geom_violin(fill="lightblue",alpha=0.3)


#####Correlation matrix of musical features
music_feature <- music_df[c(18:19,21,23:28)]
music_feature

plot(music_feature , pch=1 , cex=0.5 , col="#69b3a2")

music_cor <- cor(music_feature)
corrplot(music_cor)


#####PCA of musical features
music_pca <- prcomp(music_feature, center = TRUE,scale. = TRUE)
summary(music_pca)

year_bin <- music_df$year_bin
ggbiplot(music_pca, ellipse = TRUE, obs.scale = 1, var.scale = 1, varname.size = 4, groups = year_bin)+
  ggtitle("PCA of Musical Qualities") 


#####Percentage bar plot by cluster
cnt <- na.omit(music_df %>% group_by(year_bin, cluster) %>% summarise(n=n()))
pcnt <- do.call(rbind,
                lapply(split(cnt, cnt$year_bin), function(x){x[x$cluster=='String Lover', 'n']/sum(x$n)})
)
names(pcnt) <- 'pcnt'
pcnt$year_bin <- rownames(pcnt)
pcnt$cluster='String Lover'
pcnt2 <- do.call(rbind,
                 lapply(split(cnt, cnt$year_bin), function(x){x[x$cluster=='Poetic', 'n']/sum(x$n)})
)
names(pcnt2) <- 'pcnt'
pcnt2$year_bin <- rownames(pcnt2)
pcnt2$cluster='Poetic'
music_df <- merge(music_df, rbind(pcnt, pcnt2))

music_df$Percentage <- ifelse(music_df$cluster=='String Lover',
                              music_df$pcnt/2, 1 - music_df$pcnt/2)

ggplot(data=music_df, aes(x=year_bin, fill=cluster))  + 
  geom_bar(position="fill") +
  geom_text(aes(label = paste0(round(100*pcnt, digits = 2),"%"),y=Percentage),size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_discrete(name="Cluster types", labels=c("Poetic", "String Lover"))


##################Lyrics analysis##################

#####Sentence length distribution and pos table of song lyrics
spacy_initialize(model = "en_core_web_sm")

####50s
df1 <- music_df %>% filter(year_bin == "50s")
l1 <- df1$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')

parsed_doc1 <- spacy_parse(l1, tag = TRUE)

doc_table1 <- table(list(parsed_doc1$doc_id))
doc_sent_table1 <- table(list(parsed_doc1$doc_id, parsed_doc1$sentence_id))
doc_sent_df1 <- data.frame(doc_sent_table1)
doc_sent_df1_nozeroes <- subset(doc_sent_df1, Freq > 1)

hist(doc_sent_df1_nozeroes$Freq, breaks = nrow(doc_sent_df1_nozeroes), main="Histogram for Sentence Length (1950s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table1 <- table(parsed_doc1$pos)
pos_table1 <- prop.table(pos_table1)
pos_table1

####60s
df2 <- music_df %>% filter(year_bin == "60s")
l2 <- df2$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')%>%
  gsub(pattern = ("-"), replacement = '')

parsed_doc2 <- spacy_parse(l2, tag = TRUE)

doc_table2 <- table(list(parsed_doc2$doc_id))
doc_sent_table2 <- table(list(parsed_doc2$doc_id, parsed_doc2$sentence_id))
doc_sent_df2 <- data.frame(doc_sent_table2)
doc_sent_df2_nozeroes <- subset(doc_sent_df2, Freq > 1)

hist(doc_sent_df2_nozeroes$Freq, breaks = nrow(doc_sent_df2_nozeroes), main="Histogram for Sentence Length (1960s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table2 <- table(parsed_doc2$pos)
pos_table2 <- prop.table(pos_table2)
pos_table2

####70s
df3 <- music_df %>% filter(year_bin == "70s")
l3 <- df3$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')%>%
  gsub(pattern = ("-"), replacement = '')

parsed_doc3 <- spacy_parse(l3, tag = TRUE)

doc_table3 <- table(list(parsed_doc3$doc_id))
doc_sent_table3 <- table(list(parsed_doc3$doc_id, parsed_doc3$sentence_id))
doc_sent_df3 <- data.frame(doc_sent_table3)
doc_sent_df3_nozeroes <- subset(doc_sent_df3, Freq > 1)

hist(doc_sent_df3_nozeroes$Freq, breaks = nrow(doc_sent_df3_nozeroes), main="Histogram for Sentence Length (1970s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table3 <- table(parsed_doc3$pos)
pos_table3 <- prop.table(pos_table3)
pos_table3

####80s
df4 <- music_df %>% filter(year_bin == "80s")
l4 <- df4$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')%>%
  gsub(pattern = ("-"), replacement = '')

parsed_doc4 <- spacy_parse(l4, tag = TRUE)

doc_table4 <- table(list(parsed_doc4$doc_id))
doc_sent_table4 <- table(list(parsed_doc4$doc_id, parsed_doc4$sentence_id))
doc_sent_df4 <- data.frame(doc_sent_table4)
doc_sent_df4_nozeroes <- subset(doc_sent_df4, Freq > 1)

hist(doc_sent_df4_nozeroes$Freq, breaks = nrow(doc_sent_df4_nozeroes), main="Histogram for Sentence Length (1980s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table4 <- table(parsed_doc4$pos)
pos_table4 <- prop.table(pos_table4)
pos_table4

####90s
df5 <- music_df %>% filter(year_bin == "90s")
l5 <- df5$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')%>%
  gsub(pattern = ("-"), replacement = '')

parsed_doc5 <- spacy_parse(l5, tag = TRUE)

doc_table5 <- table(list(parsed_doc5$doc_id))
doc_sent_table5 <- table(list(parsed_doc5$doc_id, parsed_doc5$sentence_id))
doc_sent_df5 <- data.frame(doc_sent_table5)
doc_sent_df5_nozeroes <- subset(doc_sent_df5, Freq > 1)

hist(doc_sent_df5_nozeroes$Freq, breaks = nrow(doc_sent_df5_nozeroes), main="Histogram for Sentence Length (1990s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table5 <- table(parsed_doc5$pos)
pos_table5 <- prop.table(pos_table5)
pos_table5

####00s
df6 <- music_df %>% filter(year_bin == "00s")
l6 <- df6$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')%>%
  gsub(pattern = ("-"), replacement = '')

parsed_doc6 <- spacy_parse(l6, tag = TRUE)

doc_table6 <- table(list(parsed_doc6$doc_id))
doc_sent_table6 <- table(list(parsed_doc6$doc_id, parsed_doc6$sentence_id))
doc_sent_df6 <- data.frame(doc_sent_table6)
doc_sent_df6_nozeroes <- subset(doc_sent_df6, Freq > 1)

hist(doc_sent_df6_nozeroes$Freq, breaks = nrow(doc_sent_df6_nozeroes), main="Histogram for Sentence Length (2000s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table6 <- table(parsed_doc6$pos)
pos_table6 <- prop.table(pos_table6)
pos_table6

####10s
df7 <- music_df %>% filter(year_bin == "10s")
l7 <- df7$lyrics %>% gsub(pattern = ('\\\\n'), replacement = ' ') %>%
  gsub(pattern = ('\\n'), replacement = '') %>%
  gsub(pattern = ("'"), replacement = '') %>%
  gsub(pattern = ('  '), replacement = ' ')%>%
  gsub(pattern = ("'nt"), replacement = '')%>%
  gsub(pattern = ("-"), replacement = '')

parsed_doc7 <- spacy_parse(l7, tag = TRUE)

doc_table7 <- table(list(parsed_doc7$doc_id))
doc_sent_table7 <- table(list(parsed_doc7$doc_id, parsed_doc7$sentence_id))
doc_sent_df7 <- data.frame(doc_sent_table7)
doc_sent_df7_nozeroes <- subset(doc_sent_df7, Freq > 1)

hist(doc_sent_df7_nozeroes$Freq, breaks = nrow(doc_sent_df7_nozeroes), main="Histogram for Sentence Length (2010s)", 
     xlab="Sentence Length", xlim=c(0, 150))

pos_table7 <- table(parsed_doc7$pos)
pos_table7 <- prop.table(pos_table7)
pos_table7


#####Top lemmas for song lyrics
####50s
parsed_doc_nopunct1 <- subset(parsed_doc1, pos != "PUNCT")

lemmas_table_nostops1 <- table(parsed_doc_nopunct1$lemma[!parsed_doc_nopunct1$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops1 <- sort(lemmas_table_nostops1, decreasing = TRUE)
head(lemmas_table_nostops1, 50)

####60s
parsed_doc_nopunct2 <- subset(parsed_doc2, pos != "PUNCT")

lemmas_table_nostops2 <- table(parsed_doc_nopunct2$lemma[!parsed_doc_nopunct2$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops2 <- sort(lemmas_table_nostops2, decreasing = TRUE)
head(lemmas_table_nostops2, 50)

####70s
parsed_doc_nopunct3 <- subset(parsed_doc3, pos != "PUNCT")

lemmas_table_nostops3 <- table(parsed_doc_nopunct3$lemma[!parsed_doc_nopunct3$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops3 <- sort(lemmas_table_nostops3, decreasing = TRUE)
head(lemmas_table_nostops3, 50)

####80s
parsed_doc_nopunct4 <- subset(parsed_doc4, pos != "PUNCT")

lemmas_table_nostops4 <- table(parsed_doc_nopunct4$lemma[!parsed_doc_nopunct4$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops4 <- sort(lemmas_table_nostops4, decreasing = TRUE)
head(lemmas_table_nostops4, 50)

####90s
parsed_doc_nopunct5 <- subset(parsed_doc5, pos != "PUNCT")

lemmas_table_nostops5 <- table(parsed_doc_nopunct5$lemma[!parsed_doc_nopunct5$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops5 <- sort(lemmas_table_nostops5, decreasing = TRUE)
head(lemmas_table_nostops5, 50)

####00s
parsed_doc_nopunct6 <- subset(parsed_doc6, pos != "PUNCT")

lemmas_table_nostops6 <- table(parsed_doc_nopunct6$lemma[!parsed_doc_nopunct6$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops6 <- sort(lemmas_table_nostops6, decreasing = TRUE)
head(lemmas_table_nostops6, 50)

####10s
parsed_doc_nopunct7 <- subset(parsed_doc7, pos != "PUNCT")

lemmas_table_nostops7 <- table(parsed_doc_nopunct7$lemma[!parsed_doc_nopunct7$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops7 <- sort(lemmas_table_nostops7, decreasing = TRUE)
head(lemmas_table_nostops7, 50)


#####Word cloud
dfm_lm <- corpus_subset(corpus(l2)) %>% 
  dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 10, verbose = FALSE)

set.seed(120)
textplot_wordcloud(dfm_lm)


#####Topic modeling
#Converting words in lyrics into a Document Term Matrix and removing stopwords
lyrics_words=music_df%>%
  unnest_tokens(word,lyrics) %>%
  select(word,year_bin,artist) %>%
  anti_join(stop_words) %>%
  group_by(word,year_bin) %>%
  summarise(count =n())

lyrics_dtm = lyrics_words %>%
  cast_dtm(year_bin,word,count)

#Feeding the lyrics dtm to the LDA model
lyrics_lda <- LDA(lyrics_dtm, k = 2, control = list(seed = 824))
lyrics_topics <- tidy(lyrics_lda, matrix = "beta")

#Finding the top 10 most frequent words in each of the 2 topics
lyrics_top_terms <- lyrics_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

lyrics_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  labs(y="Top terms",
       title="Lyrics terms that are most common within each topic") +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Finding the top 10 terms that have the greatest difference between the 2 topics
beta_wide <- lyrics_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n = 10) %>% 
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = "Top terms",
       title="Lyrics terms that are most different between topic1 and 2")
