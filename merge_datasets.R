chil = read_csv("data/childes_english.csv") # 45k words in CHILDES
chil$wf_norm = chil$word_count / (sum(chil$word_count)/1e6) # frequency per million tokens
chil$final_count = NULL

# https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus
subt = read_csv("data/SUBTLEX-US.csv") # 60384 words appearing >1 in SUBTLEX

pos = read.csv("data/POSEnglish.csv", sep=';', stringsAsFactors = F) 
# 160k - British + American spellings..look for American POS

# minimally we want WF (per million words) for CHILDES as well as SUBTLEX, 
# along with POS for each word
# maybe we also want to limit ourselves to concrete words for some learning analyses

syll = read.csv("data/EnglishSyllables.csv", sep=';', stringsAsFactors = F) # 28k (find more?)

aoa = read.csv("data/DataAoAEnglish.csv", sep=';', 
               stringsAsFactors = F) # 31k subjective AoA ratings (from Brysbaert?) 2011?

conc = read.csv("data/DataConcrEnglish.csv", sep=';', 
                stringsAsFactors = F) # 40k concreteness ratings

val = read.csv("data/ValenceArousalRatingsEnglish.csv", sep=';', 
               stringsAsFactors = F) # 14k valence, arousal, dominance ratings

require(stringr)
# add word length
subt$length = unlist(lapply(subt$word,str_length))

length(intersect(subt$word, chil$word)) 
# 19562 words in CHILDES and in SUBTLEX

length(intersect(pos$word, subt$word)) # 46k

length(intersect(pos$word, chil$word)) # 21k

cd = subt %>% select(word, SUBTLWF, length) %>% # SUBTLWF = frequency per million words
  left_join(chil, by="word") %>%
  left_join(pos, by="word") %>% 
  left_join(conc, by="word") %>%
  left_join(val, by="word") %>%
  left_join(syll, by="word") %>%
  left_join(aoa, by="word")

summary(cd)

cd %>% filter(!is.na(SUBTLWF) & !is.na(wf_norm)) %>%
  summarize(cor = cor(SUBTLWF, wf_norm)) 
# .887 - might be interesting to look at words that are most different in relative frequency rank

dim(subset(cd, !is.na(SUBTLWF) & !is.na(wf_norm) & !is.na(PoS))) 
# 17068 words

dim(subset(cd, !is.na(SUBTLWF) & 
               !is.na(wf_norm) & 
               !is.na(PoS) & 
               !is.na(concreteness))) 
# 11116 with concreteness

dim(subset(cd, !is.na(SUBTLWF) & 
             !is.na(wf_norm) & 
             !is.na(PoS) & 
             !is.na(AoA))) 
# 9521 with AoA


dim(subset(cd, !is.na(SUBTLWF) & 
             !is.na(wf_norm) & 
             !is.na(PoS) & 
             !is.na(concreteness) & 
             !is.na(AoA))) 
# 9127 with both

save(cd, file="data/merged_data.RData")