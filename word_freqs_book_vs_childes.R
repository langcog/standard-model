require(tidyverse)
require(ggrepel)
require(here)

# SUBTLEX-US subtitle word frequencies (adult speech standard)
subf <- read_csv(here("data/SUBTLEX-US.csv")) %>%
  rename(word_count_norm = SUBTLWF, # frequency per million words
         word_count = SLUSfreq) %>% 
  mutate(source = "SUBTLEX",
         prob = word_count / sum(word_count)) %>%
  arrange(desc(word_count_norm)) %>%
  mutate(rank = 1:n()) %>%
  select(word, word_count, word_count_norm, prob, source, rank)
# hapaxes already removed  

# CHILDES corpus (hapaxes already removed, but 6295 word_count==2)
chf <- read_csv(here("data/childes_english_word_freq_cleaned_noHapaxes.csv")) %>% select(-X1) %>%
  mutate(source = "CHILDES") %>% 
  arrange(desc(word_count)) %>%
  mutate(rank = 1:n())

# 165 UK picture book corpus: https://osf.io/b2qd8/
# from: Dawson, N., Hsiao, Y., Tan, A.W.M., Banerji, N., & Nation, K. (2021). 
# Features of lexical richness in children’s books: Comparisons with child-directed speech. Language Development Research.
bbf <- read_csv(here("alvin-books/picturebook_ref.csv")) %>%
  filter(PoS_type=="lexical") %>%  # just use 9074 lexical items? (2360 non-lexical excluded)
  arrange(desc(n)) %>%
  rename(word_count = n,
         word = lemma) %>%
  mutate(rank = 1:n()) 
# only 5550 unique words (unique are lemma x PoS combos)

# Dawson2021 uses keyness scores (ratio of normalized frequency in focus corpus to norm. freq in reference corpus),
# with +10 to each words frequency 

# book corpus (Montag et al 2016)
bf <- read.csv(here("book-corpus/100out.txt"), sep=' ') %>% 
  mutate(word = ifelse(word=="i", "I", word),
         word_count_norm = word_count * (1e6 / sum(word_count)), # count per million tokens
         prob = word_count / sum(word_count),
         source = "Books"
  ) %>%
  arrange(desc(word_count)) %>%
  mutate(rank = 1:n()) 
  
# Total Words: 68103
# Total Types: 5824
nrow(subset(bf, word_count==1)) # 2548 hapaxes (44%)

cdi_voc = subset(chf, on_cdi==1)$word # 657 / 680 -- should check other 23 words

bf <- bf %>% mutate(on_cdi = ifelse(is.element(word, cdi_voc), 1, 0))
#  filter(word_count>1)

subf <- subf %>% mutate(on_cdi = ifelse(is.element(word, cdi_voc), 1, 0))



all_freq_long <- rbind(bf, chf, subf) %>%
  select(word, source, on_cdi, word_count_norm) 

# wide
all_freq <- all_freq_long %>%
  pivot_wider(names_from = source, values_from = word_count_norm) %>%
  filter(!is.na(Books), !is.na(CHILDES), !is.na(SUBTLEX)) %>%
  mutate(childes_book_ratio = CHILDES / Books,
         childes_movie_ratio = CHILDES / SUBTLEX,
         book_movie_ratio = Books / SUBTLEX)
  #mutate(childes_book_diff = CHILDES - Books)
# 4619 words with book counts 1+, CHILDES 2+
# (2887 words on both with count>1)

#summary(all_freq$childes_book_diff)
# median = -14.3, mean = -14.0
# sd = 906.5
#thresh = 2*sd(all_freq$childes_book_diff) # 1813

cor.test(all_freq$CHILDES, all_freq$Books) # .77

require(GGally)
all_freq %>% 
  mutate(Books = log(Books),
         CHILDES = log(CHILDES),
         SUBTLEX = log(SUBTLEX)) %>%
  ggpairs(columns=c("Books","CHILDES","SUBTLEX"),
        ggplot2::aes(colour=as.logical(on_cdi), alpha=.3))
ggsave(file="log_freq_books_childes_movies_by_onCDI.pdf", width=6.5, height=6.5)

ggpairs(all_freq, columns=c("childes_book_ratio","childes_movie_ratio","book_movie_ratio"),
        ggplot2::aes(colour=as.logical(on_cdi), alpha=.3))

all_rank <- rbind(bf, chf) %>%
  select(word, source, on_cdi, rank) %>%
  pivot_wider(names_from = source, values_from = rank) %>%
  filter(!is.na(Books), !is.na(CHILDES))

cor.test(all_rank$CHILDES, all_rank$Books) # .5


# ToDo: add labels for extrema (especially frequent ones)
p1 <- all_freq %>% mutate(on_cdi = factor(on_cdi)) %>%
  ggplot(aes(x=CHILDES, y=Books, color=on_cdi)) +
  geom_point(alpha=.1) + theme_classic() + 
  scale_x_log10(limits=c(.1,1e5)) + scale_y_log10(limits=c(.1,1e5)) +
  geom_smooth(method="lm") + 
  xlab("CHILDES Frequency (per million tokens)") +
  ylab("Children's Book Frequency (per million tokens)") 
  #geom_label_repel(aes(label=word), ylim=c(min(all_freq$word_count_norm),NA))

p2 <- all_freq %>% mutate(on_cdi = factor(on_cdi)) %>%
  ggplot(aes(x=CHILDES, y=SUBTLEX, color=on_cdi)) +
  geom_point(alpha=.1) + theme_classic() + 
  scale_x_log10(limits=c(.1,1e5)) + scale_y_log10(limits=c(.1,1e5)) +
  geom_smooth(method="lm") + 
  xlab("CHILDES Frequency (per million tokens)") +
  ylab("SUBTLEX Frequency (per million tokens)") 

require(ggpubr)
ggarrange(p1, p2, nrow=1, common.legend = T)
ggsave(file="freq_childes_vs_books_and_movies_by_onCDI.pdf", width=8, height=4)

all_freq %>% mutate(on_cdi = factor(on_cdi)) %>%
  filter(abs(childes_book_diff)>thresh) %>% # show the 60 words with greater than 2*SD of frequency difference bw corpora
  #arrange(desc(childes_book_diff)) %>%
  ggplot(aes(x=word, y=childes_book_diff, color=on_cdi)) +
  geom_point() + theme_classic() +
  ylab("CHILDES - Book Corpus Frequency (per million tokens)") +
  geom_hline(yintercept=0, linetype="dashed")



# re-rank after omitting words not in one corpus
all_rank %>% mutate(on_cdi = factor(on_cdi)) %>%
  arrange(CHILDES) %>% mutate(CHILDES = 1:n()) %>%
  arrange(Books) %>% mutate(Books = 1:n()) %>%
  ggplot(aes(x=CHILDES, y=Books, color=on_cdi)) +
  geom_point(alpha=.3) + theme_classic() +
  #scale_x_log10() + scale_y_log10() +
  geom_smooth(method="lm") +
  xlab("CHILDES Word Frequency Rank") +
  ylab("Book Word Frequency Rank")

# what are the least frequent CDI words?
bf %>% filter(on_cdi==1) %>% arrange(word_count_norm) # 14.68 (word_count=1 in book corpus)
chf %>% filter(on_cdi==1) %>% arrange(word_count_norm) # ~0.92 (word_count=7 in CHILDES corpus)

# is this a bit damning? CDI words are generally more frequent
# in the book corpus than in CHILDES..

all_freq_long %>% mutate(on_cdi = factor(on_cdi)) %>%
  filter(word_count_norm > 15) %>% # get rid of very low freq words (min count)
  group_by(on_cdi, source) %>%
  tidyboot::tidyboot_mean(word_count_norm) %>%
  ggplot(aes(x=source, y=empirical_stat, group=on_cdi, color=on_cdi)) + 
  geom_point() + geom_line() + geom_linerange(aes(ymin=ci_lower, ymax=ci_upper)) +
  theme_classic() + ylab("Mean Word Frequency (per million tokens)") +
  xlab("Source Corpus")
#  summarise(mean_count = mean(word_count_norm),
#            count_sd = sd(word_count_norm),
#            mean_rank = mean(rank))
#  CDI words also have higher frequency rank in books than in CHILDES (than non-CDI words)
# on_cdi source  mean_count count_sd mean_rank
#      0 Books        628.      957.      355.
#      0 CHILDES       44.      323.     4431.
#      1 Books       2130.     4991.      241.
#      1 CHILDES      995.     3175.      928.


# same minimum frequency per corpus
all_freq %>% 
  filter(Books > 15, CHILDES > 15) %>%
  

sum(all_freq$CHILDES) # 850367 / 1m words
sum(all_freq$Books) # 914850 / 1m words (because fewer hapax are removed)

require(lme4)
mdat <- rbind(bf, chf) %>% filter(word_count > 15)
#glmer(on_cdi ~ source * word_count + (1|word), family=binomial, mdat)

# which corpus freqs are most predictive of being on CDI?
m1 <- lm(on_cdi ~ CHILDES, all_freq)
# adj Rsq = .074
m2 <- lm(on_cdi ~ Books, all_freq)
# adj Rsq = .056
m3 <- lm(on_cdi ~ Books * CHILDES, all_freq)
# adj Rsq = .13

anova(m1, m2)
anova(m1, m3) # m3 preferred

# what about in top-freq book vs. childes words?
# what about by PoS?
# which specific CHILDES corpora are most predictive? (also remove the book reading ones)

summary(m3) 
# greater book freq means more likely to be selected for CDI;
# same for greater CHILDES freq; but 

# same results for just the higher than min freq words
summary(lm(on_cdi ~ Books * CHILDES, all_freq %>% filter(Books > 15, CHILDES > 15)))

# same results if scaled
summary(lm(on_cdi ~ scale(Books) * scale(CHILDES), all_freq))

# children's book corpora in other languages?
# maybe not picture books, but at least Roald Dahl BFG etc (as in Alvin's paper)

# how much is a given CDI a translation of English CDI vs. representative (based on subtitles eg)
# of that particular language
# how equivalent is the difficulty of the CDI (in terms of word frequency, not children's proportion correct)

# how to filter words (e.g., low freq), or smooth frequencies to make counts comparable?
