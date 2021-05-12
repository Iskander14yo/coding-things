import argparse
import numpy as np
import pandas as pd
import re
import sys

parser = argparse.ArgumentParser( description =
    'Считывает уникальные значения в текстовом файле. Запускать с консоли. Для справки использовать --help. '
    'Пример: python3 script.py -p "C:/Прога/Python practice/test.txt" -o 2 -t 30 -f "^M/@([A-Za-z0-9_]{1,15})/" . ')
print("Running '{}'".format(__file__))
parser.add_argument('-p', "--path", type = str, required = True, help="path to txt file")
parser.add_argument('-o', "--output_order", type=int, choices=[1,2,3], required = True, help="type of sorting")
parser.add_argument('-t', "--top", type=int, required = True, help="threshold for words` number")
parser.add_argument('-f',"--filter", required = False, help="filters the text")
args = parser.parse_args()

# читаем файл
text = open(args.path, 'rt').read()
# Подбираем только те слова, которые указали через filter (если он указан)
if args.filter :
    # Поиск по паттерну будет идти с учетом регистра, но подсчет слов будет нечувствителен к регистру
    text = re.findall(pattern = args.filter, string = text)
    text = ' '.join(text).lower().split()
    # Если вдруг не будет совпадений по нашему фильтру
    if len(text) > 0: pass
    else : sys.exit('По данному фильтру ничего не найдено')
else :
    # убрать (почти) все знаки пунктуации и сделать один регистр
    text = re.sub(r"[ :/\-,.?!;()'\"*_]+", ' ', text).lower()
    # убрать все пустые строки с табуляцией и разбить на слова
    text = re.sub(r'(\n[ \t]*)+', ' ', text).split()
values, counts = np.unique(text, return_counts=True)
# Если мы не очень волнуемся по поводу доп. расхода памяти, то удобно будет сделать словарь из массивов
diction = dict(zip(values, counts))

if args.output_order == 1:
    # Сортировка по символам в слове (затем по алфавиту).
    # list.sort() стабильна, поэтому прописывать дополнительно сортировку по алфавиту не нужно
    sortered_keys = list(diction.keys())
    sortered_keys.sort(key=len, reverse=True)
    sortered_values = [diction[k] for k in sortered_keys]
    print(pd.DataFrame(data=[sortered_keys, sortered_values],
                       index = ['Word', 'Count']).transpose().loc[:(args.top - 1)])
elif args.output_order == 2:
    # По частоте встречи
    sortered_values = list(diction.values())
    sortered_values.sort(reverse = True)
    diction1 = dict(sorted(diction.items(), key=lambda item: item[1], reverse=True))
    print(pd.DataFrame(data = [list(diction1.keys()), list(diction1.values())],
                      index = ['Word', 'Count']).transpose().loc[:(args.top - 1)])
else :
    # По алфавиту
    print(pd.DataFrame(data = [list(diction.keys()), list(diction.values())],
                      index = ['Word', 'Count']).transpose().loc[:(args.top - 1)])