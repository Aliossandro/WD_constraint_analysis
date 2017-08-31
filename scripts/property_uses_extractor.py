import os
import sys
import re
import pandas as pd



#build property list
def property_names(file_name):
    
    property_list = []
    with open('property_list.csv', 'rt') as p_list:
        for line in p_list:
            line = line.replace('\n', '')
            line = line.replace('\"', '')
            property_list.append(line)
        
        
    properties = {key : [] for key in property_list}
    for key, value in properties.iteritems():
        properties[key] = file_writer(key, file_name)

    
    return properties



###look up properties and write files
def property_uses(file_name, properties):
    
    print file_name
    file_statement = pd.read_csv(file_name, sep = '\t', header = 0,  keep_default_na=False, na_values=[""], error_bad_lines=False)
    
    
    for key, value in properties.iteritems():
        property_id = (key+"[^0-9](?:\])")
        property_file = properties[key]
        property_lines = file_statement[file_statement['comment'].str.contains(property_id, regex = True)]
        property_file = open(properties[key], 'a')
        property_lines.to_csv(property_file, sep = '\t', header = False, encoding = 'utf-8')
        #print ('Property:', key, 'Done!')
    
    print (file_name, 'done!') 
        
    

#open file
def file_opener(name_file):
    fout = open(name_file, 'w+')
    
    return fout

#write file name  
def file_writer(property_name, file_name):

    ext_name = os.path.splitext(file_name)
    part_t = ext_name[0].split('_statement')
    part_t = part_t[0].split('dumps')

    foutname = os.path.join("property_uses", property_name+"_uses.csv")    
    
    return foutname


def main():
    
    file_metadata_1 = sys.argv[1]
    
    if not os.path.exists("property_uses"):
        os.mkdir("property_uses")
    
    my_properties_1 = property_names(file_metadata_1)
    
    
    property_uses(file_metadata_1, my_properties_1)
  


if __name__ == "__main__":
    
    main()
    
    
    
    
    
