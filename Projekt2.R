#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("tidygeocoder")
#install.packages("sf")
#install.packages("mapview")

library(tidyr)
library(tidygeocoder)
library("tidyverse")
library("readxl")
library("ggplot2")


Bewerber <- read_excel("/home/jens/Documents/2021 WS Data Science II/Daten Bewerbungen/BewerberFIN2.xlsx")
names(Bewerber) <- gsub("-| ", "_", names(Bewerber)) %>% tolower() %>% gsub("[()./]", "", .)
View(Bewerber)

Bewerbungen <- read_excel("/home/jens/Documents/2021 WS Data Science II/Daten Bewerbungen/BewerbungenUml.xlsx")
names(Bewerbungen) <- gsub("-| ", "_", names(Bewerbungen)) %>% tolower() %>% gsub("[()./]", "", .)
View(Bewerbungen)

BewerMerge <- merge(x = Bewerber, y = Bewerbungen, by.x = "bewerbungen", by.y = "bewerbungs_id", all.x = TRUE) %>% 
  separate(bewerbung_für_semester, c("semester", "jahr"), sep = " ", remove = FALSE) %>% 
  mutate(master_bachelor = case_when(
    studiengang == "PPÖ"~ "Bachelor", 
    studiengang == "Man" ~ "Bachelor",
    studiengang == "PPE" ~ "Master",
    studiengang == "SO"~ "Master",
    studiengang == "GM" ~ "Master")) %>%
  select("bewerber_id", "bewerbungen", "plz", "land_wohnort", "staatsbürgerschaft", "geschlecht",
         "studiengang", "master_bachelor", "bewerbungseingang", "bewerbung_für_semester", "semester", "jahr", "vorauswahl_bestanden", 
         "aws_am", "ergebnis_aws", "zusage_für_semester", "studienplatz_angenommen", "verschoben_bis_semester", 
         "grund_für_absageverschiebung", "bewerbungsgebühr_erlassen", "doppelstudium")
View(BewerMerge)


BewerbungenPLZ <- BewerMerge %>% select("bewerber_id", "plz")
View(BewerbungenPLZ)
test_daten <- data.frame(BewerbungenPLZ = c("bewerber_id"), Ort =c("plz"))

#Adresse geocoden
test_daten_b <-BewerbungenPLZ %>%
  add_column(type = "location") %>%
  mutate(search = paste0("germany ", plz)) %>%
geocode(address = search, method = "osm", verbose = TRUE)
test_daten_b
test_daten_b$latitude
test_daten_b$longitude
View(test_daten_b)

PLZMerge <- merge(x = test_daten_b, y = BewerMerge, by.x = "bewerber_id", by.y = "bewerber_id", all.x = TRUE) %>%
  select("bewerber_id", "plz.x", "search", "lat", "long", "semester", "jahr")
View(PLZMerge)

#write.csv(PLZMerge,"/home/jens/Hackathon2021/PLZmerge.csv", row.names = FALSE)

#Analyse Bewerb pro Jahr
BewerbungenJahr <- BewerMerge %>% count(jahr)
View(BewerbungenJahr)
BewerbungsnJahrPlot <-ggplot(data=BewerbungenJahr, aes(x=jahr, y=n)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
labs(title="Bewerbungen pro Jahr gesamt", 
     x="Jahr", y = "Anz. Bewerbungen")
BewerbungsnJahrPlot



#Analyse Bewerb pro Jahr Master BA
BewerbungenJahrMABA <- BewerMerge %>% count(jahr, master_bachelor)
View(BewerbungenJahrMABA)
BewerbungsJahrMABAPlot <-ggplot(data=BewerbungenJahrMABA, aes(x=jahr, y=n, fill=master_bachelor)) +
 geom_bar(stat="identity", position=position_dodge())+
labs(title="Bewerbungen pro Jahr nach MA/BA", 
     x="Jahr", y = "Anz. Bewerbungen") +
  scale_fill_manual(values=c('black','steelblue'))+
  geom_text(aes(label=n), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_classic()
BewerbungsJahrMABAPlot


#Analyse Bewerb pro Semester
BewerbungenSemester <- BewerMerge %>% count(jahr, semester) %>% unite("jahrsemester", jahr:semester, remove = FALSE)
View(BewerbungenSemester)
BewerbungsnSemesterPlot <-ggplot(data=BewerbungenSemester, aes(x=jahrsemester, semester, y=n)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal()+
  geom_text(aes(label=n), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Bewerbungen pro Semester gesamt", 
       x="Semester", y = "Anz. Bewerbungen")
BewerbungsnSemesterPlot


#Analyse Bewerb pro Semester pro Studiengang
BewerbungenSemStud <- BewerMerge %>% count(jahr, semester, studiengang) %>% 
unite("jahrsemester", jahr:semester, remove = FALSE) %>% 
arrange(.,jahrsemester)
View(BewerbungenSemStud)
BewerbungenSemStudPlot <- ggplot(data=BewerbungenSemStud, aes(x=jahrsemester, y=n, fill=studiengang)) +
  geom_bar(stat="identity") +
 # geom_text(aes(y=, label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
  theme_minimal() +
  labs(title="Bewerbungen gesamt nach Studiengängen", 
       x="Semester", y = "Anz. Bewerbungen")
BewerbungenSemStudPlot



#Analyse Annahme Studienplatz
StudienplatzAngen <- BewerMerge %>% count(studienplatz_angenommen)
View(StudienplatzAngen)

#Analyse Geschlechterverteilung
BewerbungenGeschlecht <- BewerMerge %>% count(geschlecht) %>% replace_na(list(geschlecht="kA"))
View(BewerbungenGeschlecht)

#Analyse Geschlechterverteilung pro Jahr
BewerbungenGeschlechtJahr <- BewerMerge %>% count(geschlecht, jahr) %>% select("jahr", "geschlecht", "n")
View(BewerbungenGeschlechtJahr)
BewerbungenGeschlJahrPlot <- ggplot(data=BewerbungenGeschlechtJahr, aes(x=jahr, y=n, fill=geschlecht)) +
  geom_bar(stat="identity") +
   geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
  theme_minimal() +
  scale_fill_manual(values=c('steelblue','pink', 'purple')) +
  labs(title="Bewerbungen nach Geschlechtern", 
       x="Semester", y = "Anz. Bewerbungen")
BewerbungenGeschlJahrPlot


#Analyse Geschlechterverteilung pro Jahr
BewerbungenGeschlechtJahrAnteil <- BewerbungenGeschlechtJahr %>%
 arrange(jahr, geschlecht) %>%
 group_by(jahr)  %>%
#summarise (n=n()) %>%
 mutate(rel.freq = paste0(round(100 * n/sum(n), 1), "%"))  #%>%
 # mutate (anteil = n/sum(n))   # %>%
 #select("jahr", "geschlecht", "n", "anteil")
View(BewerbungenGeschlechtJahrAnteil)
BewerbungenGeschlJahrAnteilPlot <- ggplot(data=BewerbungenGeschlechtJahrAnteil, aes(x=jahr, y=rel.freq, fill=geschlecht)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
  theme_minimal() +
  scale_fill_manual(values=c('steelblue','pink', 'purple')) +
  labs(title="Bewerbungen nach Geschlechtern", 
       x="Semester", y = "Anz. Bewerbungen")
BewerbungenGeschlJahrAnteilPlot


#Analyse Geschlechterverteilung pro Studiengang
BewerbungenGeschlechtStudiengang <- BewerMerge %>% count(geschlecht, studiengang)
View(BewerbungenGeschlechtStudiengang)
BewerbungenGeschlStudgPlot <- ggplot(data=BewerbungenGeschlechtStudiengang, aes(x=studiengang, y=n, fill=geschlecht)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
  theme_minimal() +
  scale_fill_manual(values=c('steelblue','pink', 'purple')) +
  labs(title="Bewerbungen nach Geschlechtern", 
       x="Semester", y = "Anz. Bewerbungen")
BewerbungenGeschlStudgPlot


#Analyse Geschlechterverteilung pro Jahr
BewerbungenGeschlechtStudiengangAnteil <- BewerbungenGeschlechtStudiengang %>%
  arrange(studiengang, geschlecht) %>%
  group_by(studiengang)  %>%
  #summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 1), "%"))  #%>%
# mutate (anteil = n/sum(n))   # %>%
#select("jahr", "geschlecht", "n", "anteil")
View(BewerbungenGeschlechtStudiengangAnteil)
BewerbungenGeschlStudAnteilPlot <- ggplot(data=BewerbungenGeschlechtStudiengangAnteil, aes(x=studiengang, y=rel.freq, fill=geschlecht)) +
  geom_bar(stat="identity") +
  #apply(data, 2, function(x){x*100/sum(x,na.rm=T)})+
  geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
  theme_minimal() +
  scale_fill_manual(values=c('steelblue','pink', 'purple')) +
  labs(title="Bewerbungen nach Geschlechtern", 
       x="Semester", y = "Anz. Bewerbungen")
BewerbungenGeschlStudAnteilPlot




library(tidygeocoder)

address_single <- tibble(singlelineaddress = c(
  "germany 51103",
  "germany 12345"
)) %>% geocode(address = singlelineaddress, method = "osm", verbose = TRUE)



