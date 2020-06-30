library(LSAfun)
library(lsa)
library(xtable)
library(wordcloud)
library(kableExtra)

dir <-'C:/Users/Anas Khan/OneDrive - The University of Melbourne/Desktop/Honours Materials/Research Methods/Textual Analysis_BoQin/Reports/txt Master'

data("stopwords_en")
## Creating Term Document Matrix ##

TDM <- textmatrix(dir, stopwords =  stopwords_en, stemming = T, removeNumbers = T, minDocFreq = 3, minGlobFreq = 30)
## Weighting ##
TDM_1 <- lw_tf(TDM)
TDM_3 <- lw_tf(TDM) * gw_idf(TDM)

## Applying SVD/ Reducing dimentionality ##
LSA   <- lsa(TDM, dims = dimcalc_share())
LSA_1 <- lsa(TDM_1, dims = dimcalc_share()) 
LSA_3 <- lsa(TDM_3, dims = dimcalc_share()) 

## Labelling resulting vectors ##
tk <- t(LSA$sk * t(LSA$tk))
dk <- t(LSA$sk * t(LSA$dk))

tk_1 <- t(LSA_1$sk * t(LSA_1$tk))
dk_1 <- t(LSA_1$sk * t(LSA_1$dk))

tk_3 <- t(LSA_3$sk * t(LSA_3$tk))
dk_3 <- t(LSA_3$sk * t(LSA_3$dk))

## Identifier vectors for documents and terms ##
docs <- rownames(dk)
docs <- docs[c(1,2,3,4,64,65,66,67,52,53,54,55,24:30,6:9,38:39,63,40:43,51,59:62,5,14:17,44:47,72:74,10:13,20:23,31:33,18:19,56:58,68:71,34:37,48:50)]
terms <- rownames(tk)

docs_1 <- rownames(dk_1)
docs_1 <- docs_1[c(1,2,3,4,64,65,66,67,52,53,54,55,24:30,6:9,38:39,63,40:43,51,59:62,5,14:17,44:47,72:74,10:13,20:23,31:33,18:19,56:58,68:71,34:37,48:50)]
terms_1 <- rownames(tk_1)

docs_3 <- rownames(dk_3)
docs_3 <- docs_3[c(1,2,3,4,64,65,66,67,52,53,54,55,24:30,6:9,38:39,63,40:43,51,59:62,5,14:17,44:47,72:74,10:13,20:23,31:33,18:19,56:58,68:71,34:37,48:50)]
terms_3 <- rownames(tk_3)

## Cosine similarity matrix ##
cosine <- multicos(docs, tvectors = dk, breakdown = F)
cosine_1 <- multicos(docs_1, tvectors = dk_1, breakdown = F)
cosine_3 <- multicos(docs_3, tvectors = dk_3, breakdown = F)


## Visuals ##

term_count <- apply(TDM, 1, sum)
TCT <- t(term_count)
wordcloud(terms, TCT, min.freq = 20, random.order = FALSE, color = brewer.pal(8, 'Dark2'))

neighbors("environment", n = 10, tvectors = tk)
plot_neighbors("sustain", n = 5, tvectors = tk, col = c("black","blue","red"), dims = 3, start.lines = F,
               connect.lines = "all")
neighbors("sustain", n = 10, tvectors = tk)
plot_neighbors("emiss", n = 5, tvectors = tk, col = c("black","blue","red"), dims = 3)
neighbors("employe", n = 10, tvectors = tk)
plot_neighbors("energi", n = 5, tvectors = tk, col = c("black","blue","red"), dims = 3)
neighbors("communiti", n = 10, tvectors = tk)
plot_neighbors("environ", n = 5, tvectors = tk, col = c("black","blue","red"), dims = 3)

table <- xtable(cosine)
  
cosine[1:2,2:4]
cosine_1
cosine_3

  add_header_above(c("", "Technology" = 12)) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"), 
                position = 'center',font_size = 7 ) %>% 
  row_spec(1:12, color = 'white',background = 'red') %>% 
  pack_rows("Utilities",13,19) %>%
  pack_rows("Industrials",20,26) %>%
  pack_rows("Healthcare",27,35) %>%
  pack_rows("Financials",36,47) %>%
  pack_rows("Energy",48,58) %>%
  pack_rows("Consumer Staples",59,67) %>%
  pack_rows("Consumer Discretionary",68,74) %>%
  landscape()

  cbind(c(mean(cosine[1:12,1:12]), mean(cosine[13:19,1:12]), mean(cosine[20:26,1:12]), mean(cosine[27:35,1:12]),
          mean(cosine[36:47,1:12]), mean(cosine[48:58,1:12]), 
          mean(cosine[59:67,1:12]), mean(cosine[68:74,1:12])))
  
