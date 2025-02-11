library(tidyverse)
library(corrplot)
library(patchwork)
library(DescTools)


df = read_csv("output_data/aya_data.csv")
featural_dist = read_csv("output_data/featural_distances.csv")


### functions
get_shannon_entropy = function(vec){
  vec = vec/sum(vec) # get probability
  sums = 0
  for (i in 1:length(vec)){
    sums = sums + (vec[i] * log(vec[i]))
  }
  return(-sums)
}

get_evenness = function(vec){
  shannon = get_shannon_entropy(vec)
  return(shannon/log(length(vec)))
}

get_true_diversity = function(vec){
  return(exp(get_shannon_entropy(vec)))  
}

#### instances - resources

ggplot(data = df, aes(y = instances, x = resource, fill = resource)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) + 
  ylab("number of instances") + 
  xlab("resource level") + 
  theme(legend.position = "none") + 
  theme_bw()

df %>% group_by(resource) %>% summarise(med = median(instances), mean = mean(instances))

### phylogenetic diversity

head(df)
df$resource = factor(df$resource, levels = c("High", "Mid", "Low"))
                    
all_instances = sum(df$instances)     
all_languages = length(unique(df$language))

phyl_df = df %>%
  group_by(family, resource) %>%
  summarize(instances = sum(instances), n_langs = n(), .groups = "drop") %>%
  group_by(family) %>%
  mutate(total_instances = sum(instances), tot_langs = sum(n_langs))

p1 = ggplot(phyl_df,
       aes(y = n_langs, x = reorder(family, tot_langs), fill = family)) + 
  geom_bar(stat = "identity") +
  ylab("number of languages") + 
  xlab("language family") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none") 

p2 = ggplot(phyl_df,
  aes(y = instances, x = reorder(family, total_instances), fill = family)) + 
  geom_bar(stat = "identity") +
  ylab("number of instances") + 
  xlab("language family") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none")

p3 = ggplot(phyl_df,
       aes(y = (n_langs/all_languages)*100, x = reorder(family, tot_langs), fill = resource)) + 
  geom_bar(stat = 'identity') + 
  xlab("language families") + 
  ylab("percentage of languages (%)") + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")

p4 = ggplot(phyl_df,
       aes(y = (instances/all_instances)*100, x = reorder(family, total_instances), fill = resource)) + 
  geom_bar(stat = 'identity') + 
  xlab("language families") + 
  ylab("percent of instances (%)") + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) +
  coord_flip() + 
  theme_bw()

p3 + p4


n_lang_phyl = df %>% group_by(family) %>% summarise(n_lang = n())
n_lang_phyl = n_lang_phyl$n_lang

phyl_rich = length(n_lang_phyl)
phyl_entropy = get_shannon_entropy(n_lang_phyl)
phyl_even = get_evenness(n_lang_phyl)
phyl_true_div = get_true_diversity(n_lang_phyl)

n_instances_phyl = df %>% group_by(family) %>% summarise(instances = sum(instances))
n_instances_phyl = n_instances_phyl$instances

phyl_entropy_instances = get_shannon_entropy(n_instances_phyl)
phyl_even_instances = get_evenness(n_instances_phyl)
phyl_true_div_instances = get_true_diversity(n_instance_phyl)

phyl_entropy_instances
phyl_even_instances
phyl_true_div_instances

#### script diversity

script_df = df %>%
  group_by(script, resource) %>%
  summarize(instances = sum(instances), n_langs = n(), .groups = "drop") %>%
  group_by(script) %>%
  mutate(total_instances = sum(instances), tot_langs = sum(n_langs))

p1 = ggplot(script_df,
            aes(y = (n_langs/all_languages)*100, x = reorder(script, tot_langs), fill = resource)) + 
  geom_bar(stat = 'identity') + 
  xlab("scripts") + 
  ylab("percentage of languages (%)") + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")
p1
p2 = ggplot(script_df,
            aes(y = (instances/all_instances)*100, x = reorder(script, total_instances), fill = resource)) + 
  geom_bar(stat = 'identity') + 
  xlab("scripts") + 
  ylab("percent of instances (%)") + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) +
  coord_flip() + 
  theme_bw()
p2
p1 + p2


n_lang_script = df %>% group_by(script) %>% summarise(n_lang = n())
n_lang_script = n_lang_script$n_lang

script_rich = length(n_lang_script)
script_entropy = get_shannon_entropy(n_lang_script)
script_even = get_evenness(n_lang_script)
script_true_div = get_true_diversity(n_lang_script)

n_instances_script = df %>% group_by(script) %>% summarise(instances = sum(instances))
n_instances_script = n_instances_script$instances

script_entropy_instances = get_shannon_entropy(n_instances_script)
script_even_instances = get_evenness(n_instances_script)
script_true_div_instances = get_true_diversity(n_instances_script)

script_entropy_instances
script_even_instances
script_true_div_instances


### endangerment

df$threat_level = factor(df$threat_level)

threat_df = df %>%
  group_by(threat_level, resource) %>%
  summarize(instances = sum(instances), n_langs = n(), .groups = "drop") %>%
  group_by(threat_level) %>%
  mutate(total_instances = sum(instances), tot_langs = sum(n_langs))

p1 = ggplot(threat_df,
            aes(y = (n_langs/all_languages)*100, x = reorder(threat_level, tot_langs), fill = resource)) + 
  geom_bar(stat = 'identity') + 
  xlab("threat level") + 
  ylab("percentage of languages (%)") + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")
p1
p2 = ggplot(threat_df,
            aes(y = (instances/all_instances)*100, x = reorder(threat_level, total_instances), fill = resource)) + 
  geom_bar(stat = 'identity') + 
  xlab("threat level") + 
  ylab("percent of instances (%)") + 
  scale_fill_manual(values = c("Low" = "#F8766D", "Mid" = "#E69F00", "High" = "lightblue")) +
  coord_flip() + 
  theme_bw()
p2
p1 + p2


n_lang_threat = df %>% group_by(threat_level) %>% summarise(n_lang = n())
n_lang_threat = n_lang_threat$n_lang

threat_rich = length(n_lang_threat)
threat_entropy = get_shannon_entropy(n_lang_threat)
threat_even = get_evenness(n_lang_threat)
threat_true_div = get_true_diversity(n_lang_threat)

n_instances_threat = df %>% group_by(threat_level) %>% summarise(instances = sum(instances))
n_instances_threat = n_instances_threat$instances

threat_entropy_instances = get_shannon_entropy(n_instances_threat)
threat_even_instances = get_evenness(n_instances_threat)
threat_true_div_instances = get_true_diversity(n_instances_threat)

threat_entropy_instances
threat_even_instances
threat_true_div_instances


### featural diversity

colnames(featural_dist)[1] = "languages"

featural_dist = featural_dist %>% select(-languages)
featural_dist = as.data.frame(featural_dist)
colnames(featural_dist) = df$language
rownames(featural_dist) = df$language

dist_object = as.dist(featural_dist)
feature_vec = as.numeric(dist_object)
Desc(feature_vec)


