library(tidyverse)
library(stringr)
library(lingtypology)

df = read_csv("data/aya_languages.csv")
df = df %>% rename(ISO = "ISO Code", resource = "Resources Included")
df = df %>% rename_with(.fn = tolower, .cols = everything())

# adding glottocodes based on ISO
df$glottocode = gltc.iso(df$iso)
missing = df %>%filter(is.na(glottocode)) %>% select(iso)


# adding missing glottocodes
df[df$iso %in% missing$iso,]
df[df$iso %in% missing$iso, "glottocode"] = c("stan1318", "plat1254", "stan1306", "swah1253", "mand1415")

# Serbian is decoded as hbs in glottolog
# Kurdish is considered a language family with multiple varieties, central is national language,
# northern and southern threatened -> assuming they use central as main variety in dataset
# pashto has multiple varieties -> central is national language -> most probably used 
# standard albanian lacks entry in glottolog and is divided on tosk and gheg standard varieties. Tosk 
# is the most commonly used standard -> used ehre

#changing some as necessary
df = df %>% mutate(glottocode = case_when(
  iso == "srp" ~ "sout1528",
  iso == "kur" ~ "cent1972",
  iso == "nep" ~ "nepa1254",
  iso == "pus" ~ "sout2649",
  iso == "sqi" ~ "tosk1239",
  TRUE ~ glottocode),
  iso = case_when(
  iso == "kur" ~ "ckb",
  iso == "mlg" ~ "plt",
  iso == "msa" ~ "zsm",
  iso == "nep" ~ "npi",
  iso == "pus" ~ "pbt",
  iso == "sqi" ~ "als",
  iso == "swa" ~ "swh",
  TRUE ~ iso
))


#importing threat levels and grammar description from glottolog
df_glotto = read_csv("data/glotto_values.csv")
head(df_glotto)
df_threat = df_glotto %>% filter(Parameter_ID == "aes", Language_ID %in% df$glottocode)

# add threat lvl

df = df_threat %>% select(Language_ID, Value) %>% full_join(df, by = join_by("Language_ID" == "glottocode"))

df = df %>% rename(id = Language_ID, threat_level =  Value)



# instances in the aya dataset
aya = read_csv("data/aya_dataset.csv")
aya_instances = aya %>% group_by(language, language_code) %>% summarize(instances = n())

# joinig dialects
# arabic
arab_vari = c("arz", "ary", "ajp", "arb", "acq", "ars")
arab_instances = aya_instances %>% filter(language_code %in% arab_vari) %>% 
  mutate(language = "Arabic", language_code = "ara") %>%
  group_by(language, language_code) %>%
  summarize(instances = sum(instances))
aya_instances = aya_instances %>% filter(!language_code %in% arab_vari)
aya_instances = rbind(aya_instances, arab_instances)

#chinese
chin_instances = aya_instances %>% filter(language_code == "zho") %>% 
  mutate(language = "chinese") %>%
  group_by(language, language_code) %>%
  summarize(instances = sum(instances))
aya_instances = aya_instances %>% filter(!language_code == "zho")
aya_instances = rbind(aya_instances, chin_instances)


#merging instances
df = aya_instances %>%
  full_join(df, by = join_by("language_code" == "iso"))

df = df %>% select(!language.y)
df = df %>% rename(language = language.x)


# write csv
write.csv(df, "output_data/aya_data.csv", row.names = FALSE)
