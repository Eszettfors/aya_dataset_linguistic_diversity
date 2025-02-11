# this script requires python 3.10.16, make sure to install urielplus and pandas thorugh pip 
# it can easily be adapted to set 

from urielplus import urielplus
import pandas as pd

def get_distance_matrix(distance_type : str, langs : list):
    """takes a list of languages and string of distance type,
    calculates a distance matrix, and returns it as a 
    data frame with languages as columns and rows
    """
    matrix = u.new_distance(distance_type, langs)
    df = pd.DataFrame(matrix, columns=langs, index = langs)
    return(df)


def calc_and_write_distances(distances : list, langs : list, path: str):
    """takes a list of distances types to be calculated, a list of languages and
    a path to write to, calculates a matrix for each distancetype and outputs it
    to the path"""
    for distance in distances:
        d_matrix = get_distance_matrix(distances, langs)
        d_matrix.to_csv(f"{path}{distance}_distances.csv")
        
        
#initiate uriel+
u = urielplus.URIELPlus()
# use default features:
# union aggretaion, angular distance and fill dialect with parent values
u.set_glottocodes() # using glottocodes
u.softimpute_imputation() # impute using sofimpute

# retrieve glottocodes
langs = pd.read_csv("output_data/aya_data.csv")
glottocodes = list(langs["id"].values)

calc_and_write_distances(["featural"], glottocodes, "output_data/")

# calcualte distances, confidence and write to outputdata
#distances = ["syntactic", "phonological", "inventory", "featural"]
##for distance in distances:
 #   d_matrix = get_distance_matrix(distance, glottocodes)
#    d_matrix.to_csv(f"output_data/{distance}_distances.csv")




