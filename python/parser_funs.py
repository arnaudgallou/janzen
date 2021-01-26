from parser_pats import *
from pandas import DataFrame
import argparse
import pathlib
import regex
import csv

def sanitize(string, patterns = None):
    sub_ls = [(r" +", " "), (r"[-—–]+", "-"), (r"[`'‘’]", "'"), (r"\b(?:et|and)\b", "&"), (r"\b\p{Lu}\.\K (?=\p{Lu})|\p{Ll}\K-\n(?=\p{Ll})|\d-?\K\s(?=\d)", ""), (r"(?:\([\pL. ]+|[&)]|\bex)\K\n|\p{Ll}{2,}\K(?=\p{Lu})|\b(?:subsp|ssp|var)\.(?:\s?[a-z-]+)?\K\n|\d\K\n(?=m)", " ")]
    if patterns:
        sub_ls += patterns
    for pattern, replacement in sub_ls:
        string = regex.sub(pattern, replacement, string)
    return string

def clean_str(string):
    string = regex.sub(r"^[^A-Za-z]+|(?!\.)[^A-Za-z]+$", "", string)
    string = regex.sub(r"\s+", " ", string)
    return string

def ft_to_m(elev):
    elev = round(int(elev) * 0.3048, -1)
    return elev

def taxa_to_lower(string):
    pattern = r"^(x\s?)?[A-Z]\K\pL+\s[\pL-]+|(?:f|subsp|ssp|var)\.\s\K[\pL-]+"
    string = regex.sub(pattern, lambda x: x.group().lower(), string)
    return string

def get_taxa(string, pattern):
    if regex.search(r"^(?:x\s?)?\p{Lu}{3,}", string):
        string = taxa_to_lower(string)
    taxa = regex.search(pattern, string)[0]
    taxa = clean_str(taxa)
    return taxa

def parse_elev(string):
    res = []
    pattern = r"\d{1,5}(?:\s?-\s?\d{1,5})?\s?(?:m|ft)\b"
    elevs = regex.findall(pattern, string)
    for i in elevs:
        elev = [int(e) for e in regex.findall(r"\d+", i)]
        if i.endswith("ft"):
            elev = [ft_to_m(e) for e in elev]
        res.extend(elev)
    return res

def get_elev(string, pattern, parse_all = None, to_meter = None):
    if parse_all is None:
        elev = regex.search(pattern, string)[0]
        min_elev = regex.search(r"^\d+", elev)[0]
        max_elev = regex.search(r"\d+$", elev)[0]
    else:
        elev = parse_elev(string)
        min_elev = min(elev)
        max_elev = max(elev)
    elevs = (min_elev, max_elev)
    if parse_all is None and to_meter == "ft":
        elevs = tuple(ft_to_m(e) for e in elevs)
    return elevs

def ls_to_csv(data, file_name, cols, sep = ";"):
    df = DataFrame(data, columns = cols)
    out = df.to_csv(file_name + ".csv", sep = sep, index = False, quoting = csv.QUOTE_ALL)
    return out

def load_arguments():
  ap = argparse.ArgumentParser()
  ap.add_argument(
    "file",
    type = str,
    help = "File to parse."
  )
  ap.add_argument(
    "-e", "--parse_elev",
    nargs = "?",
    const = "",
    help = "If set, will parse all elevational values to extract the minimum and maximum elevation for each taxa."
  )
  ap.add_argument(
    "-c", "--uppercase",
    nargs = "?",
    const = ""
  )
  # ap.add_argument("-d", "--digit", nargs = "+", help = "number of digit min max elevation")
  # ap.add_argument("-l", "--level", type = str, help = "Should taxa at the genus level be extracted? Either 'genus', 'species', 'infraspecies'", default = "no")
  ap.add_argument(
    "-s", "--start",
    type = str,
    help = "Word or character string that delimites the starting point of the parser."
  )
  ap.add_argument(
    "-t", "--terminate",
    type = str,
    help = "Word or character string that delimites the ending point of the parser."
  )
  ap.add_argument(
    "-i", "--ignore",
    type = str,
    nargs = "+",
    help = "Word or character string that delimites the beginning of a section to ignore."
  )
  ap.add_argument(
    "-u", "--unit",
    type = str,
    default = "m",
    help = "Unit of elevational values used in the text. Either 'm' (the default) or 'ft'. If parse_elev is set, will automatically detect the unit and convert values to meter."
  )
  ap.add_argument(
    "-n", "--output_name",
    type = str,
    help = "Name of the output file."
  )
  return ap.parse_args()

def process_args():
    args = load_arguments()
    file = pathlib.Path(args.file)
    outfile = file_name = file.stem
    if file.suffix != ".txt":
        ap.error("The file must be in '.txt' format.")
    if args.unit not in ["m", "ft"]:
        ap.error("The unit must be 'm' or 'ft'.")
    elif args.unit == "ft":
        patterns["elev"] = regex.sub("m", "ft", patterns["elev"])
    if args.output_name is not None:
        outfile = args.output_name
    if args.uppercase is not None:
        patterns["head"] = regex.sub(r"{Ll}", "{Lu}", patterns["head"])
    out = dict(file = file, file_name = file_name, outfile = outfile, elev = patterns["elev"], head = patterns["head"])
    return out
