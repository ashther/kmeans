# -*- coding: utf-8 -*-
"""
Created on Tue Sep 15 14:06:19 2015

@author: Administrator
"""

data_tmp = []
x_index = []
y_index = []
f = open('data.txt', 'r')
for line in f.readlines():
    data_tmp.append(line.strip('\n ').split('   '))
f.close()

for da in data_tmp:
    x_index.append(float(da[0].strip(' ')))
    y_index.append(float(da[1].strip(' ')))

data = zip(x_index, y_index)