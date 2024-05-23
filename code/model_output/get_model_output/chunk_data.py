#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 29 08:18:24 2020

@author: mhbodell
"""

import os
import pandas as pd 
from pathlib import Path

print('start')

colnames = ["w_" + str(i) for i in range(0,10)]
run_nr = os.environ.get('run_nr')
input_file = os.environ.get('input_file')


folder = 'Runs/RunSuite'+ run_nr + '/Run' + run_nr + '/Spalias/'

df_chunk = pd.read_csv(str(folder) + input_file,
                       chunksize=10000,
                       header=None,
                       names=colnames,
                       delimiter = ';')

i = 0
for chunk in df_chunk:
    #print(chunk)
    chunk.to_csv('model_output/z_files/Run' + run_nr + '/chunk'+str(i)+'.csv')
    #print(i)
    i += 1
    
