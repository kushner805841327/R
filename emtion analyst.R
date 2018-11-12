# ����sentiment ���ݼ�����дʵ�
library(tidytext)
sentiments

#tidy���ṩ��get_sentiments()����
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#�����ӵ���з���
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter[\\divxlc]",ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word,text)

tidy_books

#Ϊ������䣬���ｫunnest_tokens()���������������Ϊword,��Ϊ��дʵ��ֹͣ�����ݼ���������
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word,sort = TRUE)

# �ı��Ӻδ���ʼ��ʹ��spread���������Ը��н��и�����к�������м��㣬�����㾡��з���
library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book,index = linenumber %/% 80,sentiment) %>%
  spread(sentiment,n,fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(janeaustensentiment,aes(index,sentiment,fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book,ncol = 2,scales = "free_x")

#�Ƚ�������дʵ�
pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice

#������������%/%����������и���Ĵ��½��ı�
#����ʹ����count(),spread()��mutate()��ͬ��ģʽ��������Щ���е�ÿ���ı��ľ����ֵ

afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("positive","negative"))) %>%
  mutate(method = "NRC") %>%
  count(method,index = linenumber %/% 80,sentiment) %>%
  spread(sentiment,n,fill = 0) %>%
  mutate(sentiment = positive - negative)
    
#ͨ��???ÿ����дʵ�ľ�ֵ����С˵��ÿ���ı���������Щ�����ۺ�����

bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index,sentiment,fill = method)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method,ncol = 1,scales = "free_y")

#�ʵ����浥�ʺ͸��浥�ʵķֲ�
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

#��������浥�ʺ͸��浥��
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()

bing_word_counts

#���ӻ�չ�֣���������Դ��Ϊ�����ı�����
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Contribution to sentiment",
       x= NULL)+
  coord_flip()


#�ֹ�����ͣ�ôʡ���bind_rows
custom_stop_words <- bind_rows(data_frame(word=c("miss"),
                                         lexicon = c("custom")),
                              stop_words)

custom_stop_words

#Wordcloudsģ��

library(wordcloud)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words = 100))

#��������ԣ�����comparison.cloud���������Դ���
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment,value.var = "n",fill= 0) %>%
  comparison.cloud(colors = c("gray20","gray80"),max.words = 100 )




  
  
  
  
  