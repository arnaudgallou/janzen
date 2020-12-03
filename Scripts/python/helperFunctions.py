import regex
import pandas
import csv

def sanitize(string, patterns = None):
    sub_ls = [(r' +', ' '), (r'[-—–]+', '-'), (r'\b\p{Lu}\.\K\s(?=\p{Lu})', ''), (r'\p{Ll}\K-\n(?=\p{Ll})', ''), (r'(?:\([\pL. ]+|\b(?:[&)]|ex))\K\n', ' '), (r'\p{Ll}\K(?=\p{Lu})', ' '), (r'\b(?:subsp|ssp|var)\.\s*[a-z-]+\K\n', ' '), (r'\bet\b', '&'), (r'\d\K\s(?=\d)', ''), (r'\d-\K\n(?=\d)', ''), (r'\d\K\n(?=m)', ' ')]
    if patterns:
        sub_ls += patterns
    for pattern, replacement in sub_ls:
        string = regex.sub(pattern, replacement, string)
    return string

def clean_str(string):
    string = regex.sub(r'^[^\pL]*|(?!\.)[^\pL]*$', '', string)
    string = regex.sub(r'\s+', ' ', string)
    return string

def ls_to_csv(data, file_name, cols, sep = ';'):
    df = pandas.DataFrame(data, columns = cols)
    out = df.to_csv(file_name + '.csv', sep = sep, index = False, quoting = csv.QUOTE_ALL)
    return out

def ft_to_m(elev):
    elev = round(int(elev) * 0.3048, -1)
    return elev

def parse_elev(elev_ls):
    res = []
    for i in elev_ls:
        elev = [int(e) for e in regex.findall(r'\d+', i)]
        if i.endswith('ft'):
            elev = [ft_to_m(e) for e in elev]
        res.extend(elev)
    return res