
library(gridExtra)

library(metafor)
library(meta)
library(netmeta)
library(dplyr)
library(ggplot2) 

OS <- readxl::read_xlsx("late.xlsx", sheet = "OS")
OSP <- readxl::read_xlsx("late.xlsx", sheet = "OS-IMM")
pfs <- readxl::read_xlsx("late.xlsx", sheet = "PFS")
pfspos <- readxl::read_xlsx("late.xlsx", sheet = "PFS-IMM")

#OS
mg <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
              data = OS, sm="HR", byvar = `IMM.Type`,print.byvar = FALSE)
summary(mg)

forest(mg, sortvar = TE, studlab = paste(OS$author), random = FALSE,
       overall = FALSE,rightcols = c("effect", "ci", "w.fixed")  )


#OS postive
mgosp <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                 data = OSP, sm="HR",byvar = `IMM.Type`,print.byvar = FALSE)

forest(mgosp, sortvar = TE, studlab = paste(OSP$author), random = FALSE,
       overall = FALSE,rightcols = c("effect", "ci", "w.fixed"),
       title="Hazard Ratio Plot"  )

## PFS 
mgpfs <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                 data = pfs, sm="HR",byvar = treatment,print.byvar = FALSE)
forest(mgpfs, sortvar = TE, studlab = paste(pfs$author), common = FALSE,
       overall = FALSE,rightcols = c("effect", "ci", "w.random"))

## PFS imm positive
pfsimmpos <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                     data = pfspos, sm="HR",byvar = treatment, print.byvar = FALSE)

forest(pfsimmpos,studlab = paste(pfspos$Author),overall = FALSE,rightcols = c("effect", "ci", "w.fixed"),
       random = F,sortvar = TE) 

#AE

##AE late 
AE_L <- readxl::read_xlsx("AE 11.12.xlsx", sheet = "AE-Late")
AE_L1 <- AE_L[c(1,3,4,5,6,8,9),]
meta_ae_late <- metabin(INV.E1,INV.T,CON.E1,CON.T, data = AE_L1, sm = "OR", studlab = paste(AE_L1$Author),
                        byvar=`Type`, random = F)
summary(meta_ae_late)
forest.meta(meta_ae_late, 
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))


meta_ae_late_s <- metabin(INV.E2,INV.T,CON.E2,CON.T, data = AE_L1, sm = "OR", studlab = paste(AE_L1$Author),
                          byvar=`Type`, random = F)
forest.meta(meta_ae_late_s, 
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_ae_late_s)

AE_L3 <- AE_L[c(-3,-9),]
meta_ae_late_f <- metabin(as.numeric(Fatigue.INV),INV.T,Fatigue.CON,CON.T, data = AE_L3, sm = "OR", studlab = paste(AE_L3$Author),
                          byvar=`Type`, random = F)
summary(meta_ae_late_f)
forest.meta(meta_ae_late_f, 
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

AE_L4 <- AE_L[c(-3),]
meta_ae_late_n <- metabin(as.numeric(Nausea.INV),INV.T,Nausea.CON,CON.T, data = AE_L4, sm = "OR", studlab = paste(AE_L4$Author),
                          byvar=`Type`, common = F)
summary(meta_ae_late_n)
forest.meta(meta_ae_late_n, 
            overall = FALSE,rightcols = c("effect", "ci", "w.random"))
##AE early 
AE_E <- readxl::read_xlsx("AE 11.12.xlsx", sheet = "AE-Early")[c(-1,-5),]
meta_ae_early <- metabin(INV.E1,INV.T,CON.E1,CON.T, data = AE_E, sm = "OR", studlab = paste(AE_E$Author),
                         byvar=`Type`)
forest.meta(meta_ae_early, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_ae_early)

AE_E_S <- readxl::read_xlsx("AE 11.12.xlsx", sheet = "AE-Early")[c(-5),]
meta_ae_early_s <- metabin(INV.E2,INV.T,CON.E2,CON.T, data = AE_E_S, sm = "OR", studlab = paste(AE_E_S$Author),
                           byvar=`Type`)
forest.meta(meta_ae_early_s, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_ae_early_s)

##AE early Fatigue
AE_E_F <- readxl::read_xlsx("AE 11.12.xlsx", sheet = "AE-Early")
meta_ae_early_f <- metabin(Fatigue.INV,INV.T,Fatigue.CON,CON.T, data = AE_E_F, sm = "OR", studlab = paste(AE_E_F$Author),
                           byvar=`Type`)
forest.meta(meta_ae_early_f, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

summary(meta_ae_early_f)

# AE early Nausea
meta_ae_early_n <- metabin(Nausea.INV,INV.T,Nausea.CON,CON.T, data = AE_E_F, sm = "OR", studlab = paste(AE_E_F$Author),
                           byvar=`Type`)
forest.meta(meta_ae_early_n, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_ae_early_n)

##IRAE early Pneumonitis
IRAE_E <- readxl::read_xlsx("AE 11.12.xlsx", sheet = "IRAE-Early")
IRAE_E1 <- IRAE_E[-3,]

meta_irae_early_p <- metabin(Pneumonitis.INV1,INV.T,Pneumonitis.CON1,CON.T, data = IRAE_E1, sm = "OR", studlab = paste(IRAE_E1$Author),
                             byvar=`Type`)
forest.meta(meta_irae_early_p, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_early_p)

IRAE_E1_S <- IRAE_E[c(-3,-5),]
meta_irae_early_p1 <- metabin(Pneumonitis.INV2,INV.T,Pneumonitis.CON2,CON.T, data = IRAE_E1_S, sm = "OR", studlab = paste(IRAE_E1_S$Author),
                              byvar=`Type`)
forest.meta(meta_irae_early_p1, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))


IRAE_E2 <- IRAE_E[c(-1,-4,-7),]
meta_irae_early_c <- metabin(Colitis.INV1,INV.T,Colitis.CON1,CON.T, data = IRAE_E2, sm = "OR", studlab = paste(IRAE_E2$Author),
                             byvar=`Type`)
forest.meta(meta_irae_early_c, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

meta_irae_early_h <- metabin(Hyperthyroidism.INV1,INV.T,Hyperthyroidism.CON1,CON.T, data = IRAE_E1, sm = "OR", 
                             studlab = paste(IRAE_E1$Author),byvar=`Type`)
forest.meta(meta_irae_early_h, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_early_h)

meta_irae_early_hp <- metabin(Hypothyroidism.INV1,INV.T,Hypothyroidism.CON1,CON.T, data = IRAE_E, sm = "OR", studlab = paste(IRAE_E$Author),
                              byvar=`Type`)
forest.meta(meta_irae_early_hp, common = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.random"))
summary(meta_irae_early_hp)



##IRAE late
IRAE_L <- readxl::read_xlsx("AE 11.12.xlsx", sheet = "IRAE-Late")
IRAE_L1 <- IRAE_L[-5,]
meta_irae_late <- metabin(INV.E1,INV.T,CON.E1,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                          byvar=`Type`)
forest.meta(meta_irae_late, common = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.random"))
summary(meta_irae_late)

meta_irae_late_p <- metabin(Pneumonitis.INV1,INV.T,Pneumonitis.CON1,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                            byvar=`Type`)
forest.meta(meta_irae_late_p, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

summary(meta_irae_late_p)

meta_irae_late_p1 <- metabin(Pneumonitis.INV2,INV.T,Pneumonitis.CON2,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                             byvar=`Type`)
forest.meta(meta_irae_late_p1, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_late_p1)

IRAE_L2 <- IRAE_L[c(-3,-5,-8),]
meta_irae_late_c <- metabin(Colitis.INV1,INV.T,Colitis.CON1,CON.T, data = IRAE_L2, sm = "OR", studlab = paste(IRAE_L2$Author),
                            byvar=`Type`)
forest.meta(meta_irae_late_c, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_late_c)

meta_irae_late_c1 <- metabin(Colitis.INV2,INV.T,Colitis.CON2,CON.T, data = IRAE_L2, sm = "OR", studlab = paste(IRAE_L2$Author),
                             byvar=`Type`)
forest.meta(meta_irae_late_c1, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_late_c1)

meta_irae_late_h <- metabin(Hyperthyroidism.INV1,INV.T,Hyperthyroidism.CON1,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                            byvar=`Type`)
forest.meta(meta_irae_late_h, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

summary(meta_irae_late_h)

meta_irae_late_h1 <- metabin(Hyperthyroidism.INV2,INV.T,Hyperthyroidism.CON2,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                             byvar=`Type`)
forest.meta(meta_irae_late_h1, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_late_h1)

meta_irae_late_hp <- metabin(Hypothyroidism.INV1,INV.T,Hypothyroidism.CON1,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                             byvar=`Type`)
forest.meta(meta_irae_late_hp, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))
summary(meta_irae_late_hp)

meta_irae_late_hp1 <- metabin(Hypothyroidism.INV2,INV.T,Hypothyroidism.CON2,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                              byvar=`Type`)
forest.meta(meta_irae_late_hp1, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

meta_irae_late_p <- metabin(Pneumonitis.INV1,INV.T,Pneumonitis.CON1,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                            byvar=`Type`)
forest.meta(meta_irae_late_p, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))


meta_irae_late_p1 <- metabin(Pneumonitis.INV2,INV.T,Pneumonitis.CON2,CON.T, data = IRAE_L1, sm = "OR", studlab = paste(IRAE_L1$Author),
                             byvar=`Type`)
forest.meta(meta_irae_late_p1, random = F,
            overall = FALSE,rightcols = c("effect", "ci", "w.fixed"))

### early phase for efficacy
pCR <- readxl::read_xlsx("/early.xlsx", sheet = "Sheet2")

meta2 <- metabin(INV.E,INV.T,CON.E,CON.T, data = pCR, sm = "OR")
forest.meta(meta2,common = F,
            studlab = paste(pCR$Author))
summary(meta2)

meta3 <- metabin(INV.E,INV.T,CON.E,CON.T, data = pCR, sm = "OR",byvar=IMM.Type,print.byvar = FALSE)
summary(meta3)
forest.meta(meta3,random = F,overall = FALSE,rightcols = c("effect", "ci", "w.fixed"),
            studlab = paste(pCR$Author))

me.pCR <- metabin(INV.E,INV.T,CON.E,CON.T, data = pCR, sm = "OR")
me.pCR.reg <- metareg(me.pCR, ~IMM.Type-1)
me.pCR.reg



## TNBC Late Phase
pfs.tnbc.l <-readxl::read_xlsx("TNBC.xlsx", sheet = "PFS")

mpfs.tnbc <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                     data = pfs.tnbc.l, sm="HR",byvar = IMM.Type, print.byvar = FALSE)
forest(mpfs.tnbc,common = F,overall = FALSE,rightcols = c("effect", "ci", "w.random"),
       studlab = paste(pfs.tnbc.l$Author)) 

mpfs.tnbc.reg <- metareg(mpfs.tnbc, ~IMM.Type-1)
mpfs.tnbc.reg

pfs.p.tnbc.l <-readxl::read_xlsx("TNBC.xlsx", sheet = "PFS-P")

mpfs.p.tnbc <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                       data = pfs.p.tnbc.l, sm="HR",byvar = treatment, print.byvar = FALSE)
forest(mpfs.p.tnbc,random = FALSE,overall = FALSE,rightcols = c("effect", "ci", "w.fixed"),
       studlab = paste(pfs.p.tnbc.l$Author)) 

os.tnbc.l <-readxl::read_xlsx("/Users/pearl/Desktop/免疫/TNBC.xlsx", sheet = "OS")

mos.tnbc <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                    data = os.tnbc.l, sm="HR",byvar = IMM.Type, print.byvar = FALSE )
forest(mos.tnbc,random = F,overall = FALSE,rightcols = c("effect", "ci", "w.fixed"),studlab = paste(os.tnbc.l$Author)) 

mos.tnbc.reg <- metareg(mos.tnbc, ~IMM.Type-1)
mos.tnbc.reg
bubble(mos.tnbc.reg, studlab = paste(os.tnbc.l$Author))

## TNBC late OS for IMM positive

os.p.tnbc.l <-readxl::read_xlsx("TNBC.xlsx", sheet = "OS-P")

os.p.tnbc <- metagen(log(HR),lower = log(`95CIL`), upper = log(`95CIU`),
                     data = os.p.tnbc.l, sm="HR",byvar = IMM.Type,print.byvar = FALSE)
forest(os.p.tnbc,common = F,overall = FALSE,rightcols = c("effect", "ci", "w.random"),
       studlab = paste(os.p.tnbc.l$Author)) 
## TNBC Early Phase
pCR.tnbc <-readxl::read_xlsx("TNBC.xlsx", sheet = "pCR")
mpcr.tnbc <- metabin(INV.E,INV.T,CON.E,CON.T, data = pCR.tnbc, sm = "OR")

forest.meta(mpcr.tnbc,random = F,studlab = paste(pCR.tnbc$Author))


#Net meta analysis
library(netmeta)

OS.net <- readxl::read_xlsx("./late.xlsx", sheet = "OS-Net")
OS.net$TE = log(OS.net$HR)
OS.net$seTE = (log(OS.net$`95CIU`) - log(OS.net$`95CIL`))/3.92

OSP.net <- readxl::read_xlsx("./late.xlsx", sheet = "OS-IMM-Net")
OSP.net$TE = log(OSP.net$HR)
OSP.net$seTE = (log(OSP.net$`95CIU`) - log(OSP.net$`95CIL`))/3.92

pfs.net <- readxl::read_xlsx("./late.xlsx", sheet = "PFS-Net")
pfs.net$TE = log(pfs.net$HR)
pfs.net$seTE = (log(pfs.net$`95CIU`) - log(pfs.net$`95CIL`))/3.92

pfspos.net <- readxl::read_xlsx("./late.xlsx", sheet = "PFS-IMM-Net")
pfspos.net$TE = log(pfspos.net$HR)
pfspos.net$seTE = (log(pfspos.net$`95CIU`) - log(pfspos.net$`95CIL`))/3.92

os.netmeta <- netmeta(TE = log(HR),  
                      seTE = seTE,  
                      treat1 = treat1,
                      treat2 = treat2,
                      studlab = author,
                      data = OS.net,
                      sm = "HR",
                      reference = "chemo",
                      sep.trts = " vs ")

summary(os.netmeta)

netgraph(os.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)


label_text <- paste("Heterogeneity: I^2=", formatC(os.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(os.netmeta$tau2,2), ", p=", round(os.netmeta$pval.Q,2), sep = "")

forest(os.netmeta,
       reference.group = "chemo",
       colgap.forest.left = "3.5cm",
       label.left = label_text,
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")

netrank(os.netmeta, small.values = "bad")

## os-pdl1-positive
osp.netmeta <- netmeta(TE = log(HR),  
                       seTE = seTE,  
                       treat1 = treat1,
                       treat2 = treat2,
                       studlab = author,
                       data = OSP.net,
                       sm = "HR",
                       reference.group = "chemo",
                       sep.trts = " vs ")
summary(osp.netmeta)

netgraph(osp.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)
label_text <- paste("Heterogeneity: I^2=", formatC(osp.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(osp.netmeta$tau2,2), ", p=", round(osp.netmeta$pval.Q,2), sep = "")

forest(osp.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       colgap.forest.left = "3.5cm",
       label.left = label_text,
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "random")

netrank(osp.netmeta, small.values = "bad")
## pfs
pfs.netmeta <- netmeta(TE = log(HR),  
                       seTE = seTE,  
                       treat1 = treat1,
                       treat2 = treat2,
                       studlab = author,
                       data = pfs.net,
                       sm = "HR",
                       reference.group = "chemo",
                       sep.trts = " vs ")
summary(pfs.netmeta)

netgraph(pfs.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)

label_text <- paste("Heterogeneity: I^2=", formatC(pfs.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(pfs.netmeta$tau2,2), ", p=", round(pfs.netmeta$pval.Q,2), sep = "")
forest(pfs.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.0cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "random")
netrank(pfs.netmeta, small.values = "bad")
## pfs-pdl1-positive
pfsp.netmeta <- netmeta(TE = log(HR),  
                        seTE = seTE,  
                        treat1 = treat1,
                        treat2 = treat2,
                        studlab = Author,
                        data = pfspos.net,
                        sm = "HR",
                        reference.group = "chemo",
                        sep.trts = " vs ")
summary(pfsp.netmeta)

netgraph(pfsp.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)

label_text <- paste("Heterogeneity: I^2=", formatC(pfsp.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(pfsp.netmeta$tau2,2), ", p=", round(pfsp.netmeta$pval.Q,2), sep = "")
forest(pfsp.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.0cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")

netrank(pfsp.netmeta, small.values = "bad")
# TNBC
OS.tnbc.net <- readxl::read_xlsx("./TNBC.xlsx", sheet = "OS-Net")
OS.tnbc.net$TE <- log(OS.tnbc.net$HR)
OS.tnbc.net$seTE = (log(OS.tnbc.net$`95CIU`) - log(OS.tnbc.net$`95CIL`))/3.92

OSP.tnbc.net <- readxl::read_xlsx("./TNBC.xlsx", sheet = "OS-P-Net")
OSP.tnbc.net$TE = log(OSP.tnbc.net$HR)
OSP.tnbc.net$seTE = (log(OSP.tnbc.net$`95CIU`) - log(OSP.tnbc.net$`95CIL`))/3.92

pfs.tnbc.net <- readxl::read_xlsx("./TNBC.xlsx", sheet = "PFS-Net")
pfs.tnbc.net$TE = log(pfs.tnbc.net$HR)
pfs.tnbc.net$seTE = (log(pfs.tnbc.net$`95CIU`) - log(pfs.tnbc.net$`95CIL`))/3.92

pfspos.tnbc.net <- readxl::read_xlsx("./TNBC.xlsx", sheet = "PFS-P-Net")
pfspos.tnbc.net$TE = log(pfspos.tnbc.net$HR)
pfspos.tnbc.net$seTE = (log(pfspos.tnbc.net$`95CIU`) - log(pfspos.tnbc.net$`95CIL`))/3.92

## TNBC-OS
os.tnbc.netmeta <- netmeta(TE = log(HR),  
                           seTE = seTE,  
                           treat1 = treat1,
                           treat2 = treat2,
                           studlab = Author,
                           data = OS.tnbc.net,
                           sm = "HR",
                           reference.group = "chemo",
                           sep.trts = " vs ")
summary(os.tnbc.netmeta)

netgraph(os.tnbc.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)

label_text <- paste("Heterogeneity: I^2=", formatC(os.tnbc.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(os.tnbc.netmeta$tau2,2), ", p=", round(os.tnbc.netmeta$pval.Q,2), sep = "")

forest(os.tnbc.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")
netrank(os.tnbc.netmeta, small.values = "bad")

## TNBC-OS-PDL1 positive
os.pos.tnbc.netmeta <- netmeta(TE = log(HR),  
                               seTE = seTE,  
                               treat1 = treat1,
                               treat2 = treat2,
                               studlab = Author,
                               data = OSP.tnbc.net,
                               sm = "HR",
                               reference.group = "chemo",
                               sep.trts = " vs ")
summary(os.pos.tnbc.netmeta)

netgraph(os.pos.tnbc.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)

label_text <- paste("Heterogeneity: I^2=", formatC(os.pos.tnbc.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(os.pos.tnbc.netmeta$tau2,2), ", p=", round(os.pos.tnbc.netmeta$pval.Q,2), sep = "")

forest(os.pos.tnbc.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "random")

netrank(os.pos.tnbc.netmeta, small.values = "bad")
## TNBC-PFS 
pfs.tnbc.netmeta <- netmeta(TE = log(HR),  
                            seTE = seTE,  
                            treat1 = treat1,
                            treat2 = treat2,
                            studlab = Author,
                            data = pfs.tnbc.net,
                            sm = "HR",
                            reference.group = "chemo",
                            sep.trts = " vs ")
summary(pfs.tnbc.netmeta)

netgraph(pfs.tnbc.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)

label_text <- paste("Heterogeneity: I^2=", formatC(pfs.tnbc.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(pfs.tnbc.netmeta$tau2,2), ", p=", round(pfs.tnbc.netmeta$pval.Q,2), sep = "")

forest(pfs.tnbc.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "random")


netrank(pfs.tnbc.netmeta, small.values = "bad")

## TNBC-PFS-PDL1-positive 
pfs.pos.tnbc.netmeta <- netmeta(TE = log(HR),  
                                seTE = seTE,  
                                treat1 = treat1,
                                treat2 = treat2,
                                studlab = Author,
                                data = pfspos.tnbc.net,
                                sm = "HR",
                                reference.group = "chemo",
                                sep.trts = " vs ")
summary(pfs.pos.tnbc.netmeta)

netgraph(pfs.pos.tnbc.netmeta, 
         points=T,
         plastic=F, 
         col = "#5C8286",
         col.points = "#BFBFBF",
         bg.points = "#5C8286",
         number.of.studies = T, 
         cex=1.5)

label_text <- paste("Heterogeneity: I^2=", formatC(pfs.pos.tnbc.netmeta$I2, digits=2, format="f"),  ", tau^2=", 
                    round(pfs.pos.tnbc.netmeta$tau2,2), ", p=", round(pfs.pos.tnbc.netmeta$pval.Q,2), sep = "")

forest(pfs.pos.tnbc.netmeta,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")

netrank(pfs.pos.tnbc.netmeta, small.values = "bad")


# early phase

pcr.net <- readxl::read_xlsx("./early.xlsx", sheet = "PCR-Net-1")

res <- metabin(event1, n1, event2, n2, data=pcr.net, sm="OR")

res$treat1 <- pcr.net$treat1
res$treat2 <- pcr.net$treat2
res$studlab <- pcr.net$study

early.net.m <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                       studlab=pcr.net$study, data=pcr.net, sm="OR")
netgraph(early.net.m)

summary(early.net.m)

label_text <- paste("Heterogeneity: I^2=", formatC(early.net.m$I2, digits=2, format="f"),  ", tau^2=", 
                    round(early.net.m$tau2,2), ", p=", round(early.net.m$pval.Q,2), sep = "")

forest(early.net.m,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "random")

netrank(early.net.m, small.values = "bad")

# AE
##AE late

library(netmeta)
AE.late.net <- readxl::read_xlsx("./AE 11.12.xlsx", sheet = "AE-Late-Net-1")[-2,]
res <- metabin(event1, n1, event2, n2, data=AE.late.net[,], sm="OR")

res$treat1 <- AE.late.net$treat1
res$treat2 <- AE.late.net$treat2
res$studlab <- AE.late.net$study

AE.late.net <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                       studlab=AE.late.net$study, data=AE.late.net, sm="OR")
netgraph(net)

summary(AE.late.net)

label_text <- paste("Heterogeneity: I^2=", formatC(AE.late.net$I2, digits=2, format="f"),  ", tau^2=", 
                    round(AE.late.net$tau2,2), ", p=", round(AE.late.net$pval.Q,2), sep = "")

forest(AE.late.net,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("AE immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")

netrank(AE.late.net, small.values = "bad")

##AE late severe
AE.late.s.net <- readxl::read_xlsx("./AE 11.12.xlsx", sheet = "AE-Late-Net-1")
res <- metabin(AE.late.s.net$event.s.1, AE.late.s.net$n1, AE.late.s.net$event.s.2, AE.late.s.net$n2, sm="OR")

res$treat1 <- AE.late.s.net$treat1
res$treat2 <- AE.late.s.net$treat2
res$studlab <- AE.late.s.net$study

AE.late.s.net <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                         studlab=AE.late.net$study, data=AE.late.s.net, sm="OR")
netgraph(AE.late.s.net)

summary(AE.late.s.net)

label_text <- paste("Heterogeneity: I^2=", formatC(AE.late.s.net$I2, digits=2, format="f"),  ", tau^2=", 
                    round(AE.late.s.net$tau2,2), ", p=", round(AE.late.s.net$pval.Q,2), sep = "")

forest(AE.late.s.net,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("AE immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")

netrank(AE.late.s.net, small.values = "bad")

##AE early
AE.early.net <- readxl::read_xlsx("./AE 11.12.xlsx", sheet = "AE-Early-Net-1")[-1,]
res <- metabin(AE.early.net$event1, AE.early.net$n1, AE.early.net$event2 , AE.early.net$n2, sm="OR")

res$treat1 <- AE.early.net$treat1
res$treat2 <- AE.early.net$treat2
res$studlab <- AE.early.net$study

AE.early.net <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                        studlab=AE.early.net$study, data=AE.early.net, sm="OR")
netgraph(AE.early.net)

summary(AE.early.net)

label_text <- paste("Heterogeneity: I^2=", formatC(AE.early.net$I2, digits=2, format="f"),  ", tau^2=", 
                    round(AE.early.net$tau2,2), ", p=", round(AE.early.net$pval.Q,2), sep = "")

forest(AE.early.net,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3cm",
       smlab = paste("AE immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")
netrank(AE.early.net, small.values = "bad")

##AE early severe
AE.early.s.net <- readxl::read_xlsx("./AE 11.12.xlsx", sheet = "AE-Early-Net-1")
res <- metabin(AE.early.s.net$event.s.1, AE.early.s.net$n1, AE.early.s.net$event.s.2 , AE.early.s.net$n2, sm="OR")

res$treat1 <- AE.early.s.net$treat1
res$treat2 <- AE.early.s.net$treat2
res$studlab <- AE.early.s.net$study

AE.early.s.net <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                          studlab=AE.late.net$study, data=AE.early.s.net, sm="OR")
netgraph(AE.early.s.net)

summary(AE.early.s.net)

label_text <- paste("Heterogeneity: I^2=", formatC(AE.early.s.net$I2, digits=2, format="f"),  ", tau^2=", 
                    round(AE.early.s.net$tau2,2), ", p=", round(AE.early.s.net$pval.Q,2), sep = "")

forest(AE.early.s.net,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3cm",
       smlab = paste("AE immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")
netrank(AE.early.s.net, small.values = "bad")

##IRAE late
IRAE.late.net <- readxl::read_xlsx("./AE 11.12.xlsx", sheet = "IRAE-Late-Net-1")
res <- metabin(IRAE.late.net$event1, IRAE.late.net$n1, IRAE.late.net$event2 , IRAE.late.net$n2, sm="OR")

res$treat1 <- IRAE.late.net$treat1
res$treat2 <- IRAE.late.net$treat2
res$studlab <- IRAE.late.net$study

IRAE.late.net.m <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                           studlab=IRAE.late.net$study, data=IRAE.late.net, sm="OR")
netgraph(IRAE.late.net.m)

summary(IRAE.late.net.m)

label_text <- paste("Heterogeneity: I^2=", formatC(IRAE.late.net.m$I2, digits=2, format="f"),  ", tau^2=", 
                    round(IRAE.late.net.m$tau2,2), ", p=", round(IRAE.late.net.m$pval.Q,2), sep = "")

forest(IRAE.late.net.m,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("IRAE immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "random")
netrank(IRAE.late.net.m, small.values = "bad")

## IRAE severe

IRAE.late.s.net <- readxl::read_xlsx("./AE 11.12.xlsx", sheet = "IRAE-Late-Net-1")[-5,]
res <- metabin(IRAE.late.s.net$event.s.1, IRAE.late.s.net$n1, IRAE.late.s.net$event.s.2 , IRAE.late.s.net$n2, sm="OR")

res$treat1 <- IRAE.late.s.net$treat1
res$treat2 <- IRAE.late.s.net$treat2
res$studlab <- IRAE.late.s.net$study

IRAE.late.s.net.m <- netmeta(TE=res$TE, seTE=res$seTE, treat1=res$treat1, treat2=res$treat2,
                             studlab=IRAE.late.s.net$study, data=IRAE.late.s.net, sm="OR")
netgraph(IRAE.late.s.net.m)

summary(IRAE.late.s.net.m)

label_text <- paste("Heterogeneity: I^2=", formatC(IRAE.late.s.net.m$I2, digits=2, format="f"),  ", tau^2=", 
                    round(IRAE.late.s.net.m$tau2,2), ", p=", round(IRAE.late.s.net.m$pval.Q,2), sep = "")

forest(IRAE.late.s.net.m,
       reference.group = "chemo",
       drop.reference.group = TRUE,
       label.left = label_text,
       colgap.forest.left = "3.5cm",
       smlab = paste("IRAE immunotherapy vs chemo"),
       col.square = "#5C8286",
       drop = TRUE,
       sortvar = -TE,
       pooled = "common")

netrank(IRAE.late.s.net.m, small.values = "bad")
