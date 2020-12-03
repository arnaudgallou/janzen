#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
25.05.2020 - Arnaud Gallou
pdftotext -raw -f 32 -l 284 la_amistad.pdf la_amistad.txt
"""

from regex import *
import helperFunctions as hf
import pandas
import csv

sub_ls = [
    (r'\n\K[\W\p{Greek}]+(?:[A-Z]{3,})?\W+(?=[A-Z])|([A-Z][a-z]+)\K\s\1', ''),
    (r'\n.*(?:Phytotaxa|Monro\s*ET)[^\n]+', '')
]

taxa_re = r'^[A-Z]\p{Ll}+\s[\p{Ll}-]{3,}\s*(?:[(\p{Lu}]|(?:subsp|var)\.)'
skip_re = r'^[A-Z][a-z]+\sde\b|\((?:CR|P)\)'
elev_re = r'\d+(?:\s*-\s*\d+)?(?=\s*m\b)'
tail_re = r'\d\s*m\b'

file_name = 'la_amistad'
taxa_info = ''
taxa_ls = []
j = 0

with open(file_name + '.txt', 'r') as f:
    txt = f.read()
    txt = hf.sanitize(txt, sub_ls)
    for line in txt.splitlines():
        line = line.strip()
        if search(skip_re, line):
            continue
        elif search(r'\bsp\.', line):
            j = 0
        elif search(taxa_re, line):
            taxa_info += '\n' + line + ';'
            j = 1
        elif search(tail_re, line) and j == 1:
            taxa_info += ' ' + line
            j = 0
        elif j == 1:
            taxa_info += ' ' + line
        else:
            continue
    for i in taxa_info.splitlines():
        if search(elev_re, i):
            taxa = search(r'^[^;]+', i)[0]
            elev = search(elev_re, i)[0]
            min_elev = search(r'^\d+', elev)[0]
            max_elev = search(r'\d+$', elev)[0]
            taxa_ls.append([taxa.strip(), min_elev, max_elev])

hf.ls_to_csv(taxa_ls, file_name + '.csv', cols = ['species', 'min', 'max'])