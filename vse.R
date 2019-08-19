######################################################################
########################### DATA LOADER ##############################
######################################################################

setwd("E:/papers/ICPhS2019_118212/codes_data")

dbnav = read.csv("./formant_nav.csv")
dbefl = read.csv("./formant_efl.csv")

# EXCLUDING PROBLEM SET - NOT LABELED #
dbnav = subset(dbnav, dbnav$sentencecode != "p04_003_h1")
dbefl = subset(dbefl, dbefl$sentencecode != "p04_003_h1")

table(dbnav$speakercode) # 1379 - 8명
table(dbefl$speakercode) # 1379 - 18명

length(names(table(dbnav$sentencecode))) # 108 sentence
length(names(table(dbefl$sentencecode))) # 108 sentence

table(dbnav$label)
# aa 512 / ae 768 / ao 400 / aw 104 / ax 2384 / ay 600 / eh 632 / ey 328 / ih 1432 / iy 784 / ow 472 / 
# oy 48 / uh 160 / uw 696
sum(table(dbnav$label)) # 11032

table(dbefl$label)
# aa 1152 / ae 1728 / ao 900 / aw 234 / ax 5364 /ay 1350 / eh 1422 / ey 738 / ih 3222 / iy 1764 / ow 1062 / 
# oy 108 / uh 360 / uw 1566
sum(table(dbefl$label)) # 24822

table(dbefl$level)
# AD 8274 / IH 8274 / IM 8274

# DATA SUMMATION #
sumdb = rbind(dbnav, dbefl)
sumdb$gender = as.character(sumdb$gender)
sumdb$F1 = as.numeric(as.character(sumdb$F1))
sumdb$F2 = as.numeric(as.character(sumdb$F2))
sumdb$L = as.character(sumdb$L)
sumdb$level = as.character(sumdb$level)

# DATA SEPARATION #
spk <- as.character(unique(sumdb$speakercode))
vow <- c("iy","ih","ey","eh","ae","ax","aa","ao","ow","uh","uw")
alpha = c('\u0069','\u026A','\u0065','\u025B','\u00E6','\u0259','\u0251','\u0254','\u006F','\u028A','\u0075')
L1_nav_f = subset(dbnav,dbnav$gender=="f")
L1_nav_m = subset(dbnav,dbnav$gender=="m")

L2_efl_f = subset(dbefl,dbefl$gender=="f")
L2_efl_m = subset(dbefl,dbefl$gender=="m")

level_A_m =subset(dbefl, dbefl$gender == "m" & dbefl$level == "AD")
level_A_f =subset(dbefl, dbefl$gender == "f" & dbefl$level == "AD")
level_I_m =subset(dbefl, dbefl$gender == "m" & dbefl$level == "IH")
level_I_f =subset(dbefl, dbefl$gender == "f" & dbefl$level == "IH")
level_M_m =subset(dbefl, dbefl$gender == "m" & dbefl$level == "IM")
level_M_f =subset(dbefl, dbefl$gender == "f" & dbefl$level == "IM")

######################################################################
#################### PLOT & DESCRIPTIVE STATISTICS ###################
######################################################################

# FEMALE #
par(mfrow=c(1,1))
plot(F1~F2,data = L1_nav_f, type = "n", xlim= c(2500,700), ylim = c(900,200), main = "MEAN F1-F2 (FEMALE)", xlab = "SECOND FORMANT(Hz)", ylab = "FIRST FORMANT(Hz)")
legend(x="bottomright", c("L1","L2") ,lwd = c(7,7,7), col = c("red","pink"))

# L1 #
n = 1
d_tb = rbind()
attach(L1_nav_f)
for (v in vow){
  f1 = subset(L1_nav_f$F1, label == v)
  f2 = subset(L1_nav_f$F2, label == v)
  points(mean(f2),mean(f1), pch = alpha[n],cex = 2.0,col= "red")
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1)),0),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"L1"))
  }
detach(L1_nav_f)
# L2 #
n = 1
attach(L2_efl_f)
for (v in vow){
  f1 = subset(L2_efl_f$F1, label == v)
  f2 = subset(L2_efl_f$F2, label == v)
  points(mean(f2),mean(f1), pch = alpha[n], cex = 2.0,col= "pink")
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1)),0),round(sd(f1),2),round(as.numeric(mean(f2),0)),round(sd(f2),2),"L2"))
}
# ADVANCED #
n = 1
attach(level_A_f)
for (v in vow){
  f1 = subset(level_A_f$F1, label == v)
  f2 = subset(level_A_f$F2, label == v)
  #points(mean(f2),mean(f1), pch = alpha[n],cex = 1.5, col= gray.colors(1,start = 0.6))
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1),0)),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"A"))
}
# INTERMEDIATE HIGH #|
n = 1
attach(level_I_f)
for (v in vow){
  f1 = subset(level_I_f$F1, label == v)
  f2 = subset(level_I_f$F2, label == v)
  #points(mean(f2),mean(f1), pch = alpha[n],cex = 1.5, col= gray.colors(1,start = 0.9))
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1),0)),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"I"))
}
detach(level_I_f)
# INTERMEDIATE #
n = 1
attach(level_M_f)
for (v in vow){
  f1 = subset(level_M_f$F1, label == v)
  f2 = subset(level_M_f$F2, label == v)
  #points(mean(f2),mean(f1), pch = alpha[n],cex = 1.5, col= "blue")
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1),0)),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"M"))
}
d_tb = as.data.frame(d_tb)
colnames(d_tb) = c("vowel","f1","sd_f1","f2","sd_f2","level")
d_tb$vowel = as.character(d_tb$vowel)
d_tb$f1 = as.numeric(as.character(d_tb$f1))
d_tb$f2 = as.numeric(as.character(d_tb$f2))
d_tb$sd_f1 = as.numeric(as.character(d_tb$sd_f1))
d_tb$sd_f2 = as.numeric(as.character(d_tb$sd_f2))
d_tb$level = as.character(d_tb$level)
detach(level_M_f)

write.csv(d_tb, "des_female.csv")
mean_female = d_tb

# MALE #
par(mfrow=c(1,1))
plot(F1~F2,data = L1_nav_m, type = "n", xlim= c(2500,700), ylim = c(900,200), main = "MEAN F1-F2 (MALE)", xlab = "SECOND FORMANT(Hz)", ylab = "FIRST FORMANT(Hz)")
legend(x="bottomright", c("L1","L2") ,lwd = c(7,7,7), col = c("blue","skyblue"))

# L1 #
n = 1
d_tb = rbind()
attach(L1_nav_m)
for (v in vow){
  f1 = subset(L1_nav_m$F1, label == v)
  f2 = subset(L1_nav_m$F2, label == v)
  points(mean(f2),mean(f1), pch = alpha[n],cex = 2.0,col= "blue")
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1)),0),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"L1"))
}
detach(L1_nav_m)
# L2 #
n = 1
attach(L2_efl_m)
for (v in vow){
  f1 = subset(L2_efl_m$F1, label == v)
  f2 = subset(L2_efl_m$F2, label == v)
  points(mean(f2),mean(f1), pch = alpha[n], cex = 2.0,col= "skyblue")
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1)),0),round(sd(f1),2),round(as.numeric(mean(f2),0)),round(sd(f2),2),"L2"))
}
# ADVANCED #
n = 1
attach(level_A_m)
for (v in vow){
  f1 = subset(level_A_m$F1, label == v)
  f2 = subset(level_A_m$F2, label == v)
  #points(mean(f2),mean(f1), pch = alpha[n],cex = 1.5, col= gray.colors(1,start = 0.6))
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1),0)),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"A"))
}
# INTERMEDIATE HIGH #|
n = 1
attach(level_I_m)
for (v in vow){
  f1 = subset(level_I_m$F1, label == v)
  f2 = subset(level_I_m$F2, label == v)
  #points(mean(f2),mean(f1), pch = alpha[n],cex = 1.5, col= gray.colors(1,start = 0.9))
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1),0)),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"I"))
}
detach(level_I_m)
# INTERMEDIATE #
n = 1
attach(level_M_m)
for (v in vow){
  f1 = subset(level_M_m$F1, label == v)
  f2 = subset(level_M_m$F2, label == v)
  #points(mean(f2),mean(f1), pch = alpha[n],cex = 1.5, col= "blue")
  n = n + 1
  d_tb = rbind(d_tb, c(v,as.numeric(round(mean(f1),0)),round(sd(f1),2),as.numeric(round(mean(f2),0)),round(sd(f2),2),"M"))
}
d_tb = as.data.frame(d_tb)
colnames(d_tb) = c("vowel","f1","sd_f1","f2","sd_f2","level")
d_tb$vowel = as.character(d_tb$vowel)
d_tb$f1 = as.numeric(as.character(d_tb$f1))
d_tb$f2 = as.numeric(as.character(d_tb$f2))
d_tb$sd_f1 = as.numeric(as.character(d_tb$sd_f1))
d_tb$sd_f2 = as.numeric(as.character(d_tb$sd_f2))
d_tb$level = as.character(d_tb$level)
detach(level_M_m)

write.csv(d_tb, "des_male.csv")
mean_male = d_tb

result = rbind()
for (v in vow){
  buffer1 = subset(L1_nav_f$F1,L1_nav_f$label == v)
  buffer2 = subset(L1_nav_f$F2,L1_nav_f$label == v)
  buffer3 = subset(L2_efl_f$F1, L2_efl_f$label == v)
  buffer4 = subset(L2_efl_f$F2, L2_efl_f$label == v)
  f1_result = t.test(buffer1,buffer3)$p.value
  f1_est = t.test(buffer1,buffer3)$estimate
  f2_result = t.test(buffer2, buffer4)$p.value
  f2_est = t.test(buffer2, buffer4)$estimate
  result = rbind(result, c(v,f1_est[1],f1_est[2],f1_result,f2_est[1],f2_est[2],f2_result))
}
result_female = as.data.frame(result)
colnames(result_female) = c("vowel","NAV_F1","EFL_F1","P_value_F1","NAV_F2","EFL_F2","P_value_F2")

result = rbind()
for (v in vow){
  buffer1 = subset(L1_nav_m$F1,L1_nav_m$label == v)
  buffer2 = subset(L1_nav_m$F2,L1_nav_m$label == v)
  buffer3 = subset(L2_efl_m$F1, L2_efl_m$label == v)
  buffer4 = subset(L2_efl_m$F2, L2_efl_m$label == v)
  f1_result = t.test(buffer1,buffer3)$p.value
  f1_est = t.test(buffer1,buffer3)$estimate
  f2_result = t.test(buffer2, buffer4)$p.value
  f2_est = t.test(buffer2, buffer4)$estimate
  result = rbind(result, c(v,f1_est[1],f1_est[2],f1_result,f2_est[1],f2_est[2],f2_result))
}
result_male = as.data.frame(result)
colnames(result_male) = c("vowel","NAV_F1","EFL_F1","P_value_F1","NAV_F2","EFL_F2","P_value_F2")

write.csv(result_female,"p_value_female.csv")
write.csv(result_male,"p_value_male.csv")

### speakercode - label (mean) ###
newdb = rbind()
attach(sumdb)
for (s in spk){
  for (v in vow){
    f1 = mean(sumdb$F1[sumdb$label == v & sumdb$speakercode == s])
    f2 = mean(sumdb$F2[sumdb$label == v & sumdb$speakercode == s])
    sd1 = round(sd(sumdb$F1[sumdb$label == v & sumdb$speakercode == s]),2)
    sd2 = round(sd(sumdb$F2[sumdb$label == v & sumdb$speakercode == s]),2)
    lang = (sumdb$L[sumdb$label == v & sumdb$speakercode == s])[1]
    gen = (sumdb$gender[sumdb$label == v & sumdb$speakercode == s])[1]
    lev = (sumdb$level[sumdb$label == v & sumdb$speakercode == s])[1]
    newdb = rbind(newdb, c(s,gen,v,round(f1,0),sd1,round(f2,0),sd2,lang,lev))
  }
}
newdb = as.data.frame(newdb)
colnames(newdb) = c("spk","gen","vowel","f1","f1_sd","f2","f2_sd","lang","prof")
newdb = as.data.frame(newdb)
newdb$spk = as.character(newdb$spk)
newdb$gen = as.character(newdb$gen)
newdb$lang = as.character(newdb$lang)
newdb$prof = as.character(newdb$prof)
newdb$vowel = as.character(newdb$vowel)
newdb$f1 = as.numeric(as.character(newdb$f1))
newdb$f2 = as.numeric(as.character(newdb$f2))
newdb$f1_sd = as.numeric(as.character(newdb$f1_sd))
newdb$f2_sd = as.numeric(as.character(newdb$f2_sd))

### distance in specific labels ###

centrality_set = rbind()
attach(newdb)
for(s in spk){
  buffer = subset(newdb, newdb$spk == s)
  vert = buffer$f1[buffer$vowel == "aa"] - buffer$f1[buffer$vowel == "iy"]
  hori = buffer$f2[buffer$vowel == "iy"] - buffer$f2[buffer$vowel == "ow"]
  VSE = sqrt(1-((vert*vert)/(hori*hori)))
  CP = (buffer$f2[buffer$vowel == "iy"] + buffer$f2[buffer$vowel == "ow"])/2
  MAX = buffer$f2[buffer$vowel == "iy"]
  MIN = buffer$f2[buffer$vowel == "ow"]
  VCI_ih = (buffer$f2[buffer$vowel == "ih"]-MIN)/(MAX-MIN)
  VCI_uh = (buffer$f2[buffer$vowel == "uh"]-MIN)/(MAX-MIN)
  VCI_ey = (buffer$f2[buffer$vowel == "ey"]-MIN)/(MAX-MIN)
  VCI_uw = (buffer$f2[buffer$vowel == "uw"]-MIN)/(MAX-MIN)
  gen = buffer$gen[1]
  Lang = buffer$lang[1]
  prof = buffer$prof[1]
  centrality_set = rbind(centrality_set, c(s,gen,VSE,VCI_ih,VCI_uh,VCI_ey,VCI_uw,Lang,prof))
}
final_set = as.data.frame(centrality_set)
colnames(final_set) = c("spk","gen","vse","vci_ih","vci_uh","vci_ey","vci_uw","lang","prof")
final_set$spk = as.character(final_set$spk)
final_set$vse = as.numeric(as.character((final_set$vse)))
final_set$vci_ih = as.numeric(as.character((final_set$vci_ih)))
final_set$vci_uh = as.numeric(as.character((final_set$vci_uh)))
final_set$vci_ey = as.numeric(as.character((final_set$vci_ey)))
final_set$vci_uw = as.numeric(as.character((final_set$vci_uw)))


detach(newdb)
final_set$prof = ordered(final_set$prof, levels(final_set$prof)[c(4,1,2,3)])

par(mfrow=c(1,2))
boxplot(vse~prof, data = final_set, col = rainbow(4), main = "Proficiency-VSE", ylab = "Vowel Space Eccentricity")
legend(x = "topleft", c("L1","L2_A","L2_I","L2_M"), col = rainbow(4), lwd= c(6,6,6,6))
boxplot(vse~lang, data = final_set, col = rainbow(2), main = "Language Group-VSE")
legend(x = "topleft", c("L1","L2"), col = rainbow(2), lwd= c(6,6))


library(nlme)
library(lsmeans)
m0 = lme(vse~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vse~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vse~lang, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
lsmeans(m2, pairwise~lang, adjust="tukey") # 0.0105

m0 = lme(vse~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vse~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vse~prof, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
# m2 : proficiency level (0.0037)
lsmeans(m2, pairwise~prof, adjust="tukey") # IM-null


par(mfrow=c(1,2))
boxplot(vci_ih~prof, data = final_set, col = rainbow(4), main = "Proficiency-VCI-IH", ylab = "Vowel Centrality Index-IH")
legend(x = "bottomright", c("L1","L2_A","L2_I","L2_M"), col = rainbow(4), lwd= c(6,6,6,6))
boxplot(vci_ih~lang, data = final_set, col = rainbow(2), main = "Language Group-VCI_IH")
legend(x = "bottomright", c("L1","L2"), col = rainbow(2), lwd= c(6,6,6,6))

m0 = lme(vci_ih~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_ih~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_ih~lang, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
lsmeans(m2, pairwise~lang, adjust="tukey") # <.0001

m0 = lme(vci_ih~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_ih~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_ih~prof, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
# m2 : proficiency level (<.0001)
lsmeans(m2, pairwise~prof, adjust="tukey") # null < AD,IH,IM # **

par(mfrow=c(1,2))
boxplot(vci_uh~prof, data = final_set, col = rainbow(4), main = "Proficiency-VCI-UH", ylab = "Vowel Centrality Index-UH")
legend(x = "bottomleft", c("L1","L2_A","L2_I","L2_M"), col = rainbow(4), lwd= c(6,6,6,6))
boxplot(vci_uh~lang, data = final_set, col = rainbow(2), main = "Language Group-VCI_UH")
legend(x = "bottomleft", c("L1","L2"), col = rainbow(2), lwd= c(6,6,6,6))

m0 = lme(vci_uh~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_uh~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_uh~lang, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
lsmeans(m2, pairwise~lang, adjust="tukey") # 0.0004

m0 = lme(vci_uh~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_uh~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_uh~prof, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
# m2 : proficiency level (0.0010)
lsmeans(m2, pairwise~prof, adjust="tukey") # null > AD,IH,IM *

par(mfrow=c(1,2))
boxplot(vci_ey~prof, data = final_set, col = gray.colors(4, start = 0.4, end = 0.9), main = "Proficiency-VCI-EY", ylab = "Vowel Centrality Index-EY")
legend(x = "bottomleft", c("L1","L2_A","L2_I","L2_M"), col = gray.colors(4, start = 0.4, end = 0.9), lwd= c(6,6,6,6))
boxplot(vci_ey~lang, data = final_set, col = gray.colors(2, start = 0.4, end = 0.9), main = "Language Group-VCI_EY")
legend(x = "bottomleft", c("L1","L2"), col = gray.colors(2, start = 0.4, end = 0.9), lwd= c(6,6,6,6))

m0 = lme(vci_ey~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_ey~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_ey~lang, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
lsmeans(m2, pairwise~lang, adjust="tukey") # not significant

m0 = lme(vci_ey~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_ey~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_ey~prof, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2) # not significant

par(mfrow=c(1,2))
boxplot(vci_uw~prof, data = final_set, col = gray.colors(4, start = 0.4, end = 0.9), main = "Proficiency-VCI-UW", ylab = "Vowel Centrality Index-UW")
legend(x = "bottomleft", c("L1","L2_A","L2_I","L2_M"), col = gray.colors(4, start = 0.4, end = 0.9), lwd= c(6,6,6,6))
boxplot(vci_uw~lang, data = final_set, col = gray.colors(2, start = 0.4, end = 0.9), main = "Language Group-VCI_UW")
legend(x = "bottomleft", c("L1","L2"), col = gray.colors(2, start = 0.4, end = 0.9), lwd= c(6,6,6,6))

m0 = lme(vci_uw~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_uw~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_uw~lang, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
lsmeans(m2, pairwise~lang, adjust="tukey") # not significant

m0 = lme(vci_uw~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_uw~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_uw~prof, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)


corset = cbind(final_set$vse, final_set$vci_ih,final_set$vci_uh)
colnames(corset) = c("VSE", "VCI-IH","VCI-UH")
COR = cor(corset)
write.csv(COR, "cor.csv")

cor.test(final_set$vci_uh,final_set$vci_ih,data = final_set)
# -0.64 (p.value = 0.0004296)

par(mfrow=c(1,2))
boxplot(vci_uh/vci_ih~prof, data = final_set, col = gray.colors(4, start = 0.4, end = 0.9), main = "Proficiency-(VCI-UH)/(VCI-IH)", ylab = "Ratio of VCI_UH & VCI_IH")
legend(x = "topright", c("L1","L2_A","L2_I","L2_M"), col = gray.colors(4, start = 0.4, end = 0.9), lwd= c(6,6,6,6))
boxplot(vci_uh/vci_ih~lang, data = final_set, col = gray.colors(2, start = 0.4, end = 0.9), main = "Language Group-(VCI-UH)/(VCI-IH)")
legend(x = "topright", c("L1","L2"), col = gray.colors(2, start = 0.4, end = 0.9), lwd= c(6,6,6,6))

m0 = lme(vci_uh/vci_ih~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_uh/vci_ih~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_uh/vci_ih~lang, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
lsmeans(m2, pairwise~lang, adjust="tukey") # <.0001

m0 = lme(vci_uh/vci_ih~1, random = ~1|spk, data = final_set, method = "ML")
m1 = lme(vci_uh/vci_ih~gen, random = ~1|spk, data = final_set, method = "ML")
m2 = lme(vci_uh/vci_ih~prof, random = ~1|spk, data = final_set, method = "ML")
anova(m0,m1,m2)
# m2 : proficiency level (<.0001)
lsmeans(m2, pairwise~prof, adjust="tukey") # null > AD,IH,IM ***

