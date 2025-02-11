from datasets import load_dataset
import pandas as pd

# loads data
ds = load_dataset("CohereForAI/aya_dataset", "default")

# merges train and test data
train = pd.DataFrame(ds["train"])
test = pd.DataFrame(ds["test"])
aya = pd.concat(objs = [train, test], axis = 0)

#write dataset as .csv to data
aya.to_csv("data/aya_dataset.csv", index = False)