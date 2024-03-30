import pandas as pd
import json
from 


class DataframeConverter:
    def __init__(self, json_file):
        self.file = json_file
    def flatten_json(self, data):
        res = {}
        def flatten(x, name=''):
            if type(x) is dict:
                for a in x:
                    flatten(x[a], name + a + '_')
            elif type(x) is list:
                i = 0
                for a in x:
                    flatten(a, name + str(i) + '_')
                    i += 1
            else:
                res[name[:-1]] = x
        flatten(data)
        return res
    

    def unnest_wider(self):
        with open(self.file) as f:
            data = json.load(f)
            data = self.flatten_json(data)
            
        