## Landings para WGCEPH2024

# Entre mudanças de base de dados, inclusao de esforço no IC e reconstrução de séries desde 2007,
# que grande coboiada que praqui vai.

# 1 - Comparar desembarques oficiais com estimativas de desembarque vendas-dia

options(scipen=999)
library(dplyr)
library(ggplot2)
source('C://repos/path.R'); path('remote')
land = read.csv(paste0(dados, 'desembarques//desemb_pescart.csv'),
                sep = ",", dec = ".") %>% 
  mutate(CODFAO = gsub(" ", "", CODFAO))
         # EARTE = case_when(EARTE == 3 ~ 'OTB',
         #                   EARTE == 5 ~ 'PS',
         #                   EARTE == 13 ~ 'MIS_MIS'))
# land_ceph = read.csv(paste0(dados, 'desembarques//desemb_pescart_cephs.csv'),
#                 sep = ",", dec = ".") %>% 
#   mutate(CODFAO = gsub(" ", "", CODFAO),
#          EARTE = case_when(EARTE == 3 ~ 'OTB',
#                            EARTE == 5 ~ 'PS',
#                            EARTE == 13 ~ 'MIS_MIS'))




fao = c('COM','IIL','SQC','OCT','OMZ','CTC',
  'OCC','EOI','SQF','OFJ','OUL','OUW','SQI','SQR','OUM','OCM','OCZ','SQE',
  'SQM','YHT','OQD','SQZ','CTL','TDQ','EDT','SQA','OJJ','SQL','CTR','SQU')


# Testes com os vendas-dia

## 2014
ano = 2011

assign(paste0('l_',ano),
land %>% 
  filter(IANO == ano) %>% 
  filter(CODFAO %in% fao),
envir = .GlobalEnv)

load(paste0(dados,
            'vendas-dia/vd_',
            ano,'.Rdata'))


assign(paste0('teste_',ano),
  get(paste0('l_',ano)) %>% 
  # filter(CODFAO == 'OCC') %>% 
  group_by(EARTE, CODFAO) %>% 
  summarise(DGRM = sum(QESTIMADA)/1000) %>% 
  full_join(get(paste0('vd_',ano)) %>% 
              filter(EESPECIE %in% fao) %>%
              group_by(EESPECIE, EGRUPART) %>% 
              summarise(VD = sum(QVENDA)/1000),
            by = c('EARTE' = 'EGRUPART', 'CODFAO' = 'EESPECIE')),
 envir = .GlobalEnv)


ggplot(get(paste0('teste_',ano)),
       aes(x = DGRM, y = VD, label = CODFAO)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) + 
  geom_label(size = 2) + 
  theme_light() + 
  facet_grid(EARTE~.) + 
  labs(title = ano)




# Esforço
# Truque desta porra: =SUMPRODUCT($F$3:$F$85;U3:U85)

## Importado dos ficheiros da anaclaudia
eff_2017_otb = c(377191.02,	493060.08,	528667.18,	531837.43,	625891.31,	632292.6,	
                 598235.83,	644739.61,	458304.55,	522066.86,	648917.02,	507136.98)


eff_2018_otb = c(381782.79, 553844.4, 361335.86, 591484.42, 678998.07, 615657.79,	
                 629923.5,	636089.19,	585944.97,	562302.07,	616083.22,	490958.91)


eff_2019_otb = c(380019.72,	556086.54,	546200.9,	534997.05,	653688.15,	611779.68,
                 618916.01,	619984.51,	584020.52,	626603.27,	535101.15,	413464.09)


eff_2020_otb = c(415755.33,	592467.89,	499867.28,	406003.28,	556921.97,	594070.02,
                 625816.43,	625791.31,	609973.44,	589952.74,	534219.12,	449839.87)

#estes dados foram compilados das colunas mais à direita (corrigidos)
eff_2021_otb = c(330691.65,	529048.52,	600447.96,	589681.13,	668378.81,	648442.64,
                 653517.72,	683334.84,	678622.36,	649475.02,	657045.63,	542219.97)

#estes dados foram tirados do separador kw_dia_prt
eff_2022_otb = c(409828.64,	590481.81,	502122.73,	467467.71,	671088.46,	600995.18,
                 99825.38,	639218.6,	604762.2,	562030.2,	571539.3,	469840.41)

eff_2023_otb = c(365089.13,	542691.96,	575226.87,	439673.38,	673302,	598206.35,
                 657755.42,	657517.59,	627709.53,	540380.84,	606937.37,	497296.51)

sum(eff_2023_otb)  ## confirmacao se esta ok


eff_n =
rbind(eff_2017_otb,
      eff_2018_otb,
      eff_2019_otb,
      eff_2020_otb,
      eff_2021_otb,
      eff_2022_otb,
      eff_2023_otb) %>% data.frame %>% 
  cbind(c(2017:2023)) %>%
  cbind(rep("OTB",7)) %>% 
  cbind(rep("27.9.a.c.n",7))

names(eff_n) = c(1:12,'year',"metier",'zona')

eff_s =
  rbind(eff_2017_otb,
        eff_2018_otb,
        eff_2019_otb,
        eff_2020_otb,
        eff_2021_otb,
        eff_2022_otb,
        eff_2023_otb) %>% data.frame %>% 
  cbind(c(2017:2023)) %>%
  cbind(rep("OTB",7)) %>% 
  cbind(rep("27.9.a.c.s",7))

names(eff_s) = c(1:12,'year',"metier",'zona')

eff_a =
  rbind(eff_2017_otb,
        eff_2018_otb,
        eff_2019_otb,
        eff_2020_otb,
        eff_2021_otb,
        eff_2022_otb,
        eff_2023_otb) %>% data.frame %>% 
  cbind(c(2017:2023)) %>%
  cbind(rep("OTB",7)) %>% 
  cbind(rep("27.9.a.s.a",7))

names(eff_a) = c(1:12,'year',"metier",'zona')

eff = rbind(eff_n, eff_s, eff_a)

eff = reshape2::melt(eff, id = c('metier', 'zona', 'year'))
names(eff) = c('metier', 'zona', 'year', 'mes', 'effort')
eff$mes = as.numeric(as.character(eff$mes))

# usado apenas em 2024 enquanto nao ha dados
eff$effort = NA

# Importa dados
# land = read.csv(paste0(dados, 'desembarques//desemb_2022.csv'),
#                sep = ",", dec = ".")

# load("C:/dados_pnab/vendas-dia/vd_2022.Rdata")

# land = vd_2022 %>%
#   mutate(ANO = year_sale,
#             MES = month_sale,
#             zona = zona,
#             PORTO_PNAB = IPORTO,
#             PORTO_SLV = IPORTO,
#             PORTO_NOME = PORTO,
#             ARTE_EU = EGRUPART,
#             COD_FAO = EESPECIE,
#             DESEMBARQUE = QVENDA) %>% 
#   group_by(ANO, MES, zona, PORTO, ARTE_EU, COD_FAO) %>% 
#   summarise(QESTIMADA = sum(DESEMBARQUE))


# slv = read.csv(paste0(dados, 'especies_slv/slv.csv'))
load(paste0(dados, 'especies_slv/especies_slv.Rdata')); slv = especies_slv; rm(especies_slv)
portos = read.csv(paste0(dados, 'portos_slv//codigos_portos.csv'))
portos[182,] = c(40950, "CAIS DO BICO", "csbic", "NW","AVEIRO", "PTCDB", "CDB",NA,NA)


# acrescenta codigos fao da tabela SLV 
# land = merge(land,slv[,c("ESPECIE_SLV","COD_FAO","FAMILIA")],all.x=T,all.y=F,
#              by.x = 'IESPECIE',
#              by.y ="ESPECIE_SLV")

# land = merge(land,slv[,c("IESPECIE","CODFAO","COD_FAMILIA")],all.x=T,all.y=F,
#              by.x = 'IESPECIE',
#              by.y ="IESPECIE")


# acrescenta portos slv
land =merge(land,portos[,c("codporto","nome","zona")],
      all.x = T,
      all.y = F,
      by.x = "IPORTO",
      by.y = "codporto")

# restringe desembarques a codigos fao tirados do ASFIS
## no dia 26.04 foi acrescentado o SQU, que foi alterado para OMZ
fao = c("SQC","OCT","OMZ","CTC",
        "OCC","EOI","SQF","OFJ",
        "OUL","OUW","SQI","SQR",
        "OUM","OCM","OCZ","SQE",
        "SQM","YHT","OQD","SQZ",
        "CTL","TDQ","EDT","SQA",
        "OJJ","SQL","CTR","SQU")

ano = 2023

# transforma tabela
land_export =
land %>%
  select(nome, zona, CODFAO, IANO, IMES, EARTE, QESTIMADA) %>%
  filter(IANO == ano) %>%
  # remove artes espanholas
  filter(EARTE %in% unique(land$EARTE)[!grepl("SP_", unique(land$EARTE))]) %>%
  filter(CODFAO %in% fao) %>%
  mutate(zona = factor(case_when(zona == "NW" ~ "27.9.a.c.n",
                                 zona == "SW" ~ "27.9.a.c.s",
                                 zona == 'S' ~ "27.9.a.s.a",
                                 zona == 'ACORES' ~ '27.10',
                                 T ~ 'O')),
         # Acerta niveis com formato intercatch
         EARTE = factor(case_when(EARTE == 3 ~ "OTB",
                                    EARTE == 5 ~ 'PS_SPF_0_0_0',
                                    T ~ 'MIS_MIS_0_0_0'))) %>%
  group_by(CODFAO, zona, IMES, EARTE) %>%
  # desembarques à zona, em kg
  summarise(QESTIMADA = sum(QESTIMADA, na.rm = T)) %>% 
  filter(zona != 'O')



# Quando desembarques se obtiverem a partir das vendas-dia:
# land_export =
#   get(paste0('vd_', ano)) %>% 
#   mutate(EESPECIE = case_when(EESPECIE %in% fao ~ EESPECIE,
#                               T ~ 'OTH')) %>% 
#   group_by(year_sale, zona, EESPECIE, month_sale, EGRUPART) %>% 
#   summarise(QVENDA = sum(QVENDA))
# 
# effort = 
#   get(paste0('vd_', ano)) %>% 
#   group_by(year_sale, zona, month_sale, EGRUPART, PORTO, IDATVEND, IEMBARCA) %>% 
#   summarise(kw_day = unique(Power.Main))
# Fim do troço vendas-dia


# land_export = land %>% filter(COD_FAO %in% fao)

# altera 'SQU' para 'OMZ' para ficar de acordo com o nome do stock do ICES
land_export[land_export$CODFAO == 'SQU',]$CODFAO = 'OMZ'
land_export$zona = as.character(land_export$zona)

# save(land, file="C://Google Drive//Polvices//WGCEPH 2020//desemb_mes_2019.Rdata")
# write.csv(land, file = 'intercatch_2024_raw.csv')


#corrido temporariamente enquanto nao mandaram dataset completo do esforço
# land_export = land_export %>% 
#   filter(EARTE == 'OTB')

#cria ficheiro IC
for(j in unique(land_export$CODFAO)){
  teste_9 = data.frame()
  teste_10 = data.frame()
  occ = land_export[land_export$CODFAO==j,]
  for(i in 1:nrow(occ)){
    linha =
      paste("HI,","PT,",ano,",","Month,",
        occ$IMES[i],",",
        occ$EARTE[i],",",
        ifelse(occ$zona[i] == '27.10',"SubArea,","AreaUnit,"),
        occ$zona[i],",",
        "NA,","NA,",eff[eff$metier==occ$EARTE[i] &
                          eff$mes == occ$IMES[i] &
                          eff$zona == occ$zona[i] &
                          eff$year == ano,]$effort,",kWd",
        "\n",
        "SI,","PT,",ano,",","Month,",
        occ$IMES[i],",",
        occ$EARTE[i],",",
        ifelse(occ$zona[i] == '27.10',"SubArea,","AreaUnit,"),
        occ$zona[i],",",
        "NA,",
        occ$CODFAO[i],",",
        "NA,","L,","R,","NA,","H,","U,","NA,","t,",
        occ$QESTIMADA[i]/1000,",",
        occ$QESTIMADA[i]/1000,",",
        "-9,",",,",
        sep="")
    if(occ$zona[i] != '27.10'){teste_9[nrow(teste_9)+1,1] = linha}
    else if(occ$zona[i] == '27.10'){teste_10[nrow(teste_10)+1,1] = linha}
  }
  
if(nrow(teste_9>0)){  
write.table(teste_9,
            file=paste0('Intercatch/IC',ano,j,"27_9a_PT_landings.dat",sep="_")
            ,sep="",row.names = F,col.names = F,quote=F)}

if(nrow(teste_10>0)){  
write.table(teste_10,
            file=paste0('Intercatch/IC',ano,j,"27_10_PT_landings.dat",sep="_")
            ,sep="",row.names = F,col.names = F,quote=F)}
}

# resumo de desembarques OTB
land_export %>% 
  filter(ARTE_EU == 'MIS_MIS_0_0_0') %>% 
  group_by(CODFAO) %>%
  summarise(tons = sum(QESTIMADA/1000))
