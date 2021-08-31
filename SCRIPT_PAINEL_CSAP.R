#####Preparo ----
### Pacotes, funções e agrupamentos essenciais
##Instalando pacotes necessários
if (!require("pacman")) install.packages("pacman") #Garante que o pacman está instalado

pacman::p_load(dplyr, grid, tidyr, tidyverse, cssTools, read.dbc, geobr, ggplot2, purrr,
               foreign, data.table, devtools, DT, readxl, rmarkdown, bslib, flextable, flexdashboard)

devtools::install_github("fulvionedel/csapAIH")
library(csapAIH)

#Funções essenciais para leitura de arquivos em formato .dbc/.csv
ler_dbc <- function(file){
  df <- read.dbc(file)
  df$fileName <- file
  return(df)
}

ler_csv <- function(file){
  df <- read.csv(file, h=F, sep=",", encoding="UTF-8")
  df$fileName <- file
  return(df)
}


##Classificações e agrupamentos necessários para visualização em RS e Faixa Etária
#Regiões de Saúde
central <- c("Plano Piloto", "Lago Norte", "Lago Sul", "Varjao", "Varjão", "Cruzeiro", "Sudoeste/", "Sudoeste e Octogonal")
centro.sul <- c("Guara", "Guará", "Candangolândia", "Candangolandia", "Nucleo Bandeirante", "Núcleo Bandeirante", "Riacho Fundo I", "Riacho Fundo II", "Riacho Fundo", "Park Way", "SIA", "SCIA-Estrutural")
norte <- c("Planaltina", "Sobradinho", "Sobradinho II", "Fercal")
sul <- c("Gama", "Santa Maria")
leste <- c("Paranoa", "Paranoá", "Itapoa", "Itapoã", "Jardim Botanico", "Jardim Botânico", "São Sebastião", "Sao Sebastiao")
oeste <- c("Ceilandia", "Ceilândia", "Sol Nascente/", "Sol Nascente e Pôr do Sol", "Brazlandia", "Brazlândia")
sudoeste <- c("Taguatinga", "Vicente Pires", "Águas Claras", "Aguas Claras", "Arniqueira", "Recanto das Emas", "Samambaia")

#Faixa Etária
jovem <- c("0-4",  "5-9", "10-14", "15-19")
adulto <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
idoso <- c("50-54", "55-59", "60-64" ,"65-69", "70-74", "75-79" ,"80 +" )

##Cria automaticamente pasta local para salvar as figuras dos gráficos
if (!dir.exists("Figuras")) dir.create("Figuras")

####Tratamento -----
###Bancos de dados

##Banco de dados do SIH/AIH
#Juntando arquivos separados com padrão de nome 
junta_aih_df <-list.files(pattern="^RDDF20..\\.dbc")

##Lendo os arquivos .dbc
aih_df <- junta_aih_df %>% map_dfr(ler_dbc)

##Banco de dados de CEP do DF
#Juntando arquivos separados com padrão de nome 
junta_cep_df <- list.files(pattern="^df.cepaberto_parte_.\\.csv")

#Lendo os arquivos .csv e nomeando a primeira coluna
cep_df <- junta_cep_df %>% map_dfr(ler_csv)
names(cep_df) <- c("CEP", "Endereço", "Complemento", "RA", "Cod.Cidade", "Cod.Estado", "arquivo")


#Padronizando os nomes das RA e juntando às RS
cep_ra<-cep_df%>% #tabela
  mutate(RA=ifelse(grepl("Estrutural", RA), "SCIA-Estrutural", RA),
         RA=ifelse(grepl("Arniqueira", RA)|RA=="Setor Habitacional Arniqueira", "Arniqueira", RA),
         RA=ifelse(grepl("Águas Claras", RA), "Águas Claras", RA),
         RA=ifelse(grepl("Lago Norte", RA)|RA=="Setor de Habitações Individuais Norte"|RA=="Granja do Torto", "Lago Norte", RA),
         RA=ifelse(grepl("Guará", RA), "Guará", RA),
         RA=ifelse(grepl("Zona Industrial", RA), "SIA", RA),
         RA=ifelse(grepl("Itapoã", RA),"Itapoã", RA),
         RA=ifelse(grepl("Lago Sul", RA)|RA=="Setor de Habitações Individuais Sul", "Lago Sul", RA),
         RA=ifelse(grepl("Jardim Botânico", RA), "Jardim Botânico", RA),
         RA=ifelse(grepl("São Sebastião", RA)|grepl("Bora Manso",RA)|grepl("Fazenda Papuda",RA),"São Sebastião", RA),
         RA=ifelse(grepl("Núcleo Bandeirante", RA), "Núcleo Bandeirante", RA),
         RA=ifelse(grepl("Planaltina", RA), "Planaltina", RA),
         RA=ifelse(grepl("Paranoá", RA), "Paranoá", RA),
         RA=ifelse(grepl("Santa Maria", RA), "Santa Maria", RA),
         RA=ifelse(grepl("Taguatinga", RA), "Taguatinga", RA),
         RA=ifelse(grepl("Fercal", RA)|RA=="Setor Habitacional Fercal", "Fercal", RA),
         RA=ifelse(grepl("Sobradinho II", RA), "Sobradinho II", RA),
         RA=ifelse(grepl("Sobradinho", RA)&!grepl("II", RA), "Sobradinho", RA),
         RA=ifelse(grepl("Vila Rabelo II", RA),"Sobradinho", RA),
         RA=ifelse(grepl("Riacho Fundo II", RA), "Riacho Fundo II", RA),
         RA=ifelse(grepl("Riacho Fundo", RA)&!grepl("II", RA), "Riacho Fundo", RA),
         RA=ifelse(grepl("Brazlândia", RA), "Brazlândia" , RA),
         RA=ifelse(grepl("Gama", RA), "Gama" , RA),
         RA=ifelse(grepl("Samambaia", RA), "Samambaia" , RA),
         RA=ifelse(grepl("Sol", RA), "Sol Nascente e Pôr do Sol" , RA),
         RA=ifelse(grepl("Ceilândia", RA), "Ceilândia" , RA),
         RA=ifelse(grepl("Planaltina", RA), "Planaltina" , RA),
         RA=ifelse(grepl("Vicente Pires", RA), "Vicente Pires" , RA),
         RA=ifelse(grepl("Park Way", RA), "Park Way" , RA),
         RA=ifelse(grepl("Cruzeiro", RA), "Cruzeiro" , RA),
         RA=ifelse(grepl("Recanto das Emas", RA), "Recanto das Emas" , RA),
         RA=ifelse(grepl("Candangolândia", RA), "Candangolândia" , RA),
         RA=ifelse(grepl("Sudoeste/", RA)|grepl("Sudoeste", RA)|grepl("Octogonal", RA), "Sudoeste e Octogonal" , RA),
         RA=ifelse(grepl("Asa Norte", RA)|grepl("Asa Sul", RA)|grepl("Setor Noroeste", RA)|grepl("Setor Militar Urbano", RA)|grepl("Setores Complementares", RA)|grepl("Telebrasília", RA)|grepl("Campus Universitário Darcy Ribeiro",RA)|grepl("Telebrasília",RA)|grepl("Setor Policial Sul",RA)|grepl("Zona Cívico-Administrativa",RA)|grepl("Vila Planalto",RA), "Plano Piloto" , RA),
         RA=ifelse(grepl("Varjão", RA), "Varjão" , RA))%>%
  mutate(CEP=as.character(CEP))%>%
  filter(RA!="")%>%
  select(CEP, RA)%>%
  mutate(RS=ifelse(RA %in% central, "Central", NA), 
         RS=ifelse(RA %in% centro.sul, "Centro-Sul", RS),
         RS=ifelse(RA %in% sul, "Sul", RS),
         RS=ifelse(RA %in% oeste, "Oeste", RS), 
         RS=ifelse(RA %in% leste, "Leste", RS),
         RS=ifelse(RA %in% norte, "Norte", RS),
         RS=ifelse(RA %in% sudoeste, "Sudoeste", RS))


###Trabalhando com as internações por CSAP
#Classificando no banco de dados o que é CSAP/não-CSAP
aih_classi <- csapAIH(aih_df)%>% 
  rename(CEP=cep)%>%
  mutate(CEP=as.character(CEP))

#Juntando os bancos de dados
aih_cep <- left_join(aih_classi, cep_ra)%>%
  separate(data.inter, c("ano", "mes", "dia"), sep="-")%>%
  filter(!is.na(RA))%>%
  mutate(fxetar5=as.character(fxetar5))%>%
  mutate(faixa=ifelse(fxetar5 %in% jovem, "Jovem",
                      ifelse(fxetar5 %in% adulto, "Adulto", "Idoso")))%>%
  mutate(grupo_nome=ifelse(grupo=="g01","1.Prev. vacinação", NA),
         grupo_nome=ifelse(grupo=="g02","2.Gastroenterite", grupo_nome),
         grupo_nome=ifelse(grupo=="g03","3.Anemia", grupo_nome),
         grupo_nome=ifelse(grupo=="g04","4.Def. nutricion", grupo_nome),
         grupo_nome=ifelse(grupo=="g05","5.Infec. ouvido, nariz e garganta", grupo_nome),
         grupo_nome=ifelse(grupo=="g06","6.Pneumonias bacterianas", grupo_nome),
         grupo_nome=ifelse(grupo=="g07","7.Asma", grupo_nome),
         grupo_nome=ifelse(grupo=="g08","8.Pulmonares", grupo_nome),
         grupo_nome=ifelse(grupo=="g09","9.Hipertensão", grupo_nome),
         grupo_nome=ifelse(grupo=="g10","10.Angina", grupo_nome),
         grupo_nome=ifelse(grupo=="g11","11.Insuf. cardíaca", grupo_nome),
         grupo_nome=ifelse(grupo=="g12","12.Cerebrovasculares", grupo_nome),
         grupo_nome=ifelse(grupo=="g13","13.Diabetes mellitus", grupo_nome),
         grupo_nome=ifelse(grupo=="g14","14.Epilepsias", grupo_nome),
         grupo_nome=ifelse(grupo=="g15","15.Infec. urinária", grupo_nome),
         grupo_nome=ifelse(grupo=="g16","16.Infec. pele e subcutâneo", grupo_nome),
         grupo_nome=ifelse(grupo=="g17","17.D. infl. órgãos pélvicos femininos", grupo_nome),
         grupo_nome=ifelse(grupo=="g18","18.Úlcera gastrointestinal", grupo_nome),
         grupo_nome=ifelse(grupo=="g19","19.Pré-natal e parto", grupo_nome),
         grupo_nome=ifelse(grupo=="não-CSAP", "Não-CSAP", grupo_nome))



#Separando só as variáveis de interesse
#Todas as internações
aih_cep_resumo <- aih_cep%>%
  group_by(RA, RS, mes, sexo, faixa, grupo_nome, csap)%>%
  summarize(Casos=n())


#Internações por CSAP
csap_cep_resumo <- aih_cep%>%
  filter(grupo_nome!="Não-CSAP")%>%
  group_by(RS, RA, mes, sexo, faixa, grupo_nome, csap)%>%
  summarize(Casos=n())


####Criação -----
###DF
##Geral
prop_intern_df <- aih_cep%>% #tabela
  group_by(csap)%>%
  summarize(N=n())%>%
  mutate(Tot=sum(N),
         Prop=N/Tot*100)

as.data.frame(prop_intern_df)

#Proporção
prop_intern_df%>% #gráfico
  ggplot(aes(x=Prop, y=csap, fill=csap, label = round(Prop,1)))+
  geom_bar(stat="identity", position="dodge", color="#008080", fill=c("#4169E1", "#40E0D0"), size=0.3, width=.4)+
  geom_text(position = position_dodge(width = .2),
            vjust=1,
            size = 3, fontface="bold") + 
  theme_bw()+
  scale_y_discrete(name="Causa da internação", labels=(c("CSAP", "Não-CSAP")))+
  scale_x_continuous(name="Proporção")+
  labs(title="Proporção de internações por tipo de causa, DF, 2020")+
  coord_flip()+
  ggsave(paste0("Figuras/Prop_DF.png"), width=200, height=100, units="mm")

#N Absoluto
prop_intern_df%>% 
ggplot(aes(x=N, y=csap, fill=csap, label=N))+
  geom_bar(stat="identity", position="dodge", color="#008080", fill=c("#4169E1", "#40E0D0"), size=0.3, width=.4)+
  geom_text(position = position_dodge(width = .2),
            vjust = 1,
            size = 3, fontface="bold") + 
  theme_bw()+
  scale_y_discrete(name="Causa da internação", labels=(c("CSAP", "Não-CSAP")))+
  scale_x_continuous(name="Internações")+
  labs(title="Internações por tipo de causa, DF, 2020")+
  coord_flip()+
  ggsave(paste0("Figuras/N_DF.png"), width=200, height=100, units="mm")


  
##Região de Saúde
prop_intern_geral<-aih_cep_resumo%>% #tabela
    group_by(RS, csap)%>%
    summarize(N=n())%>%
    mutate(Tot=sum(N),
           Prop=N/Tot*100) 
  
as.data.frame(prop_intern_geral)

#Proporção
prop_intern_geral%>% #gráfico
  ggplot(aes(x=RS, y=Prop, fill=csap, label = round(Prop,1)))+
  geom_bar(stat="identity", position="stack", color="#008080", size=0.4)+
  geom_text(position = position_stack(vjust = .5),
            size = 3, fontface="bold")+ 
  theme_bw()+
  scale_fill_manual(values=c("#40E0D0", "#4169E1"), name = "Causa de internação", labels = c("Não-CSAP", "CSAP"))+
  scale_x_discrete(name="Região de Saúde")+
  scale_y_continuous(name="Proporção")+
  labs(title = "Proporção de internação por tipo de causa, segundo RS, DF, 2020")+
  ggsave(paste0("Figuras/Prop_DF_RS.png"), width=200, height=100, units="mm")


#N Absoluto
aa<-prop_intern_geral%>%
  ggplot(aes(x=RS, y=N, fill=csap))+
  geom_bar(stat="identity", position="dodge", color="#008080", size=0.4)+
  geom_text(aes(label=N), position=position_dodge(width=0.5), vjust=2, size=3, fontface="bold")+ 
  theme_bw()+
  scale_fill_manual(values=c("#40E0D0", "#4169E1"), name = "Causa de internação", labels = c("Não-CSAP", "CSAP"))+
  scale_x_discrete(name="Região de Saúde")+
  scale_y_continuous(name="Internações")+
  labs(title = "Internações por tipo de causa, segundo RS, DF, 2020 ")+
  ggsave(paste0("Figuras/N_DF_RS.png"), width=200, height=100, units="mm")

##Mês
prop_intern_df_mes<-aih_cep_resumo%>% #tabela
    group_by(mes, csap)%>%
    summarize(N=n())%>%
    mutate(Tot=sum(N),
           Prop=N/Tot*100)

as.data.frame(prop_intern_df_mes)

#Proporção
prop_intern_df_mes%>% #gráfico
    ggplot(aes(x=factor(mes), y=Prop, group=csap, color=csap))+
    geom_line(size=1)+
    geom_text(aes(label=round(Prop,1)), size = 4, fontface="bold", check_overlap=T, show.legend=F)+ 
    theme_bw()+
    labs(title="Proporção de internações por tipo de causa, DF, 2020", x="Mês", y="Proporção", color="Tipo de causa")+
    scale_colour_manual(labels=(c("Não-CSAP", "CSAP")), values=c("#40E0D0", "#4169E1"))+
    ggsave(paste0("Figuras/Prop_DF_Mes.png"), width=200, height=100, units="mm")
  
#N Absoluto
prop_intern_df_mes%>% #gráfico
      ggplot(aes(x=factor(mes), y=N, group=csap, color=csap, label=N))+
      geom_line(size=1)+
      geom_text(size = 4, fontface="bold", check_overlap=T, show.legend=F)+ 
      theme_bw()+
      labs(title="Internações por tipo de causa, DF, 2020", x="Mês", y="Internações", color="Tipo de causa")+
      scale_colour_manual(labels=(c("Não-CSAP", "CSAP")), values=c("#40E0D0", "#4169E1"))+
      ggsave(paste0("Figuras/N_DF_Mes.png"), width=200, height=100, units="mm")
    
    
##Grupo      
prop_grupo_geral<-csap_cep_resumo%>% #tabela
        group_by(grupo_nome)%>%
        summarize(N=n())%>%
        mutate(Tot=sum(N),
               Prop=N/Tot*100)

as.data.frame(prop_grupo_geral)

#Proporção
prop_grupo_geral%>% #gráfico
        ggplot(aes(x=reorder(grupo_nome, Prop), y=Prop, label = round(Prop,1)))+
        geom_bar(stat="identity", position="dodge", color="#008080", size=0.4, fill="#28D7FA")+
        geom_text(position = position_dodge(width = .5), hjust=1,
                  size = 4, fontface="bold") + 
        theme_bw()+
        scale_y_continuous(name="Proporção")+
        scale_x_discrete(name="Grupo")+
        labs(title="Proporção de internações por CSAP, segundo grupo de causa, DF, 2020")+
        coord_flip()+
        ggsave(paste0("Figuras/Prop_Grupo.png"), width=250, height=210, units="mm")
    
#N Absoluto     
prop_grupo_geral%>% #gráfico
        ggplot(aes(x=reorder(grupo_nome, N), y=N, label=N))+
        geom_bar(stat="identity", position="stack", color="#008080", size=0.4, fill="#28D7FA")+
        geom_text(position = position_dodge(width = .5), hjust=1,
                  size = 4, fontface="bold") + 
        theme_bw()+
        scale_y_continuous(name="Internações")+
        scale_x_discrete(name="Grupo")+
        labs(title="Internações por CSAP, segundo grupo de causa, DF, 2020")+
        coord_flip()+
        ggsave(paste0("Figuras/N_Grupo.png"), width=250, height=210, units="mm")
      

###Regiões de Saúde 
  
#Lista RAs para iterar a criação de gráficos
csap_cep_lista_RS <- split(aih_cep[, -ncol(aih_cep)], aih_cep$RS)

#Automatizar cálculo para as RSs
RSs<-unique(prop_intern_geral$RS)  
 
#Mês e RA 

prop_RA_mes<-aih_cep_resumo%>% #tabela
  group_by(RS, RA, mes)%>%
  summarize(N=n())%>%
  mutate(Tot=sum(N),
         Prop=N/Tot*100)

as.data.frame(prop_RA_mes)

#Proporção
for (rs in RSs) {
  dado<-prop_RA_mes%>%
    filter(RS==rs)
  graf<-dado%>%
  ggplot(aes(x=factor(mes), y=Prop, group=RA, color=RA))+
  geom_line(size=1)+
  labs(title = "Proporção de internações por CSAP por mês, segundo RA, DF, 2020 ",
       x = "Mês",
       y = "Proporção")+
  geom_text(aes(label=round(Prop,1)), size = 4, fontface="bold", check_overlap=T, show.legend=F)+
  theme_bw()+
  scale_colour_manual(values=c("#29EE75", "#2098E3", "#3FB2BF", "#223C8F", "#23EAF4", "#758B69", "#5FD41F", "#2A9740"))
  ggsave(paste0("Figuras/Prop_",rs,"_Mes.png"), width=200, height=150, units="mm")
}

#N Absoluto
for (rs in RSs) {
  dado<-prop_RA_mes%>%
    filter(RS==rs)
  graf<-dado%>%
    ggplot(aes(x=factor(mes), y=N, group=RA, color=RA, label=N))+
    geom_line(size=1)+
    labs(title = "Internações por CSAP por mês, segundo RA, DF, 2020 ",
         x = "Mês",
         y = "Internações")+
    geom_text(size = 4, fontface="bold", check_overlap=T, show.legend=F)+
    theme_bw()+
    scale_colour_manual(values=c("#29EE75", "#2098E3", "#3FB2BF", "#223C8F", "#23EAF4", "#758B69", "#5FD41F", "#2A9740"))+
    ggsave(paste0("Figuras/N_",rs,"_Mes.png"), width=200, height=150, units="mm")
}


#RA e sexo
prop_RS_RA_sexo<-csap_cep_resumo%>% #tabela
  group_by(RS, RA, sexo)%>%
  summarize(N=n())%>%
  mutate(Tot=sum(N),
         Prop=N/Tot*100)

as.data.frame(prop_RS_RA_sexo)

for (rs in RSs) {
  dado<-prop_RS_RA_sexo%>% #gráfico
    filter(RS==rs)
  graf<-dado%>%
  ggplot(aes(x=RA, y=Prop, fill=sexo, label = round(Prop,1)))+
  geom_bar(stat="identity", position="stack", width=0.5, color="#008080", size=0.4)+
  geom_text(position = position_stack(vjust = .5), check_overlap =T, size = 4, fontface="bold")+
  theme_bw()+
  scale_y_continuous(name="Proporção")+
  scale_x_discrete(name="Região Administrativa")+
  scale_fill_manual(values=c("#98FB98", "#87CEEB"),
                    name = "Sexo", labels=c("Masculino", "Feminino"))+
  labs(title="Proporção de internações por CSAP, segundo RA e sexo, DF, 2020")+
  ggsave(paste0("Figuras/Prop_",rs,"_Sexo.png"), width=220, height=150, units="mm")
}

#N Absoluto
for (rs in RSs) {
  dado<-prop_RS_RA_sexo%>% #gráfico
    filter(RS==rs)
  graf<-dado%>%
  ggplot(aes(x=RA, y=N, fill=sexo, label =N))+
  geom_bar(stat="identity", position="stack", width=0.5, color="#008080", size=0.4)+
  geom_text(position = position_stack(vjust = .5), check_overlap =T, size = 4, fontface="bold")+
  theme_bw()+
  scale_y_continuous(name="Internações")+
  scale_x_discrete(name="Região Administrativa")+
  scale_fill_manual(values=c("#98FB98", "#87CEEB"),
                    name = "Sexo", labels=c("Masculino", "Feminino"))+
  labs(title="Internações por CSAP, segundo RA e sexo, DF, 2020")+
ggsave(paste0("Figuras/N_",rs,"_Sexo.png"), width=220, height=150, units="mm")
}

  
#RA e faixa etária
prop_RS_RA_faixa<-csap_cep_resumo%>% #tabela
  group_by(RS, RA, faixa)%>%
  summarize(N=n())%>%
  mutate(Tot=sum(N),
         Prop=N/Tot*100)

as.data.frame(prop_RS_RA_faixa)

#Proporção
for (rs in RSs) {
  dado<-prop_RS_RA_faixa%>%
    filter(RS==rs)
  graf<-dado%>%
    mutate(faixa=factor(faixa, levels=c("Jovem", "Adulto", "Idoso")))%>%
  ggplot(aes(x=RA, y=Prop, fill=faixa, label = round(Prop,1)))+
  geom_bar(stat="identity", position="stack", width=0.5, color="#008080", size=0.4)+
  geom_text(position = position_stack(vjust = .5), check_overlap=T,
            size = 3.2, fontface="bold") + 
  theme_bw()+
  scale_x_discrete(name="Região Administrativa")+
  scale_y_continuous(name="Proporção")+
  scale_fill_manual(values=c("#00FFFF", "#66CDAA", "#00FA9A"),
                    name = "Faixa")+
  labs(title="Proporção de internações por CSAP, segundo RA e faixa etária, DF, 2020")+
  coord_flip()+
  ggsave(paste0("Figuras/Prop_",rs,"_Faixa.png"), width=220, height=150, units="mm")
}

  
#N Absoluto
for (rs in RSs) {
  dado<-prop_RS_RA_faixa%>%
    filter(RS==rs)
  graf<-dado%>%
    mutate(faixa=factor(faixa, levels=c("Jovem", "Adulto", "Idoso")))%>%
  ggplot(aes(x=RA, y=N, fill=faixa, label = round(N)))+
  geom_bar(stat="identity", position="stack", width=0.5, color="#008080", size=0.4)+
  geom_text(position = position_stack(vjust = .5), check_overlap=T,
            size = 3, fontface="bold") + 
  theme_bw()+
  scale_x_discrete(name="Região Administrativa")+
  scale_y_continuous(name="Internações")+
  scale_fill_manual(values=c("#00FFFF", "#66CDAA", "#00FA9A"),
                    name = "Faixa")+
  labs(title="Internações por CSAP, segundo RA e faixa etária, DF, 2020")+
  coord_flip()+
  ggsave(paste0("Figuras/N_",rs,"_Faixa.png"), width=220, height=150, units="mm")
}

#RA e grupo CSAP
prop_grupo_RS_RA<-csap_cep_resumo%>% #tabela
  filter(!is.na(grupo_nome))%>%
  group_by(RS, RA, grupo_nome)%>%
  summarize(N=n())%>%
  mutate(Tot=sum(N),
         Prop=N/Tot*100)

as.data.frame(prop_grupo_RS_RA)

#Proporção
for (rs in RSs) {
  dado<-prop_grupo_RS_RA%>%
    filter(RS==rs)
  graf<-dado%>%
    ggplot(aes(x=reorder(grupo_nome, Prop), y=Prop, label = round(Prop,1)))+
    geom_bar(stat="identity", position="dodge", color="#008080", size=0.5, fill="#00BFFF")+
    geom_text(position = position_dodge(width = 1), 
              hjust=0.7,
              size = 2.8, fontface="bold") + 
    theme_bw()+
    scale_y_continuous(name="Região Administrativa")+
    scale_x_discrete(name="Grupo")+
    labs(title="Proporção de internações por CSAP, segundo RA e grupo de causa, DF, 2020")+
    coord_flip()+
    facet_wrap(~RA, ncol=6)+
    ggsave(paste0("Figuras/Prop_",rs,"_Grupo.png"), width=250, height=150, units="mm")
}

#N Absoluto
for (rs in RSs) {
  dado<-prop_grupo_RS_RA%>%
    filter(RS==rs)
  graf<-dado%>%
    ggplot(aes(x=reorder(grupo_nome, N), y=N, label =N))+
    geom_bar(stat="identity", position="dodge", color="#008080", size=0.5, fill="#00BFFF")+
    geom_text(position = position_dodge(width = 1), 
              hjust=0.7, size = 2.8, fontface="bold") + 
    theme_bw()+
    scale_y_continuous(name="Internações por CSAP")+
    scale_x_discrete(name="Grupo")+
    labs(title="Internações por CSAP, segundo RA e grupo de causa, DF, 2020")+
    coord_flip()+
    facet_wrap(~RA, ncol=6)+
    ggsave(paste0("Figuras/N_",rs,"_Grupo.png"), width=250, height=150, units="mm")
}
