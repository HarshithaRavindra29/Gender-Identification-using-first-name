# -------------------
# Author: Harshitha Ravindra
# Date: Nov 09, 2018
# Analysis : Gender classification using names
# -------------------

library(data.table)
library(stringi)
library(dplyr)
library(naivebayes)
set.seed(42)


data_in = fread('names_ethnea_genni_country_sample.csv')
data_in = data_in[Genni != '-',]
data_in[,First:= trimws(gsub("_","",First))]
data_in[,last_2_char := stri_sub(First,from = -2, to = -1)]
data_in[, count_terms := .N, by= .(Ethnea, PubCountry, Genni, last_2_char)]
data_in = data_in[count_terms > 5,]
head(data_in)
data_in[,c("First","AUID","Last","count_terms"):=NULL]
data_in = data.table(sapply(data_in, tolower))

Train = sample_n(data_in,0.8*nrow(data_in))


sid = as.numeric(rownames(Train))
Test = data_in[-sid,]
Test_x = Test[,-c("Genni")]
nb = naive_bayes(Genni ~ ., data = Train)
pred2 = predict(nb,Test_x )
Test[,gen_pred := pred2]
Test[,Truth := ifelse(Genni== 'm' & gen_pred== 'm', 'TP',ifelse(Genni== 'm' & gen_pred== 'f', 'FN',ifelse(
  Genni== 'f' & gen_pred=='m', 'FP','TN'
)))]
truth_table = data.table(table(Test$Truth))

Tp= truth_table[V1 == 'TP',N]
Fn= truth_table[V1 == 'FN',N]
Fp= truth_table[V1 == 'FP',N]
Tn= truth_table[V1 == 'TN',N]


Model_metrics = function(Tp, Fn, Fp,Tn){
Total_out = (Tp+Tn+Fp+Fn)
Accuracy = (Tp+Tn)/Total_out
Exp_Po = (Tp+Fn)/Total_out*(Tp+Fp)/Total_out
Exp_No = (Fp+Fn)/Total_out*(Tn+Fn)/Total_out
Exp = Exp_Po+Exp_No
kappa_val = (Accuracy - Exp)/(1-Exp)
return(list(Accuracy,kappa_val))
}

Accuracy= Model_metrics(Tp,Fn,Fp,Tn)[[1]]
Kappa_val = Model_metrics(Tp,Fn,Fp,Tn)[[2]]
Accuracy
Kappa_val