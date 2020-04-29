import os, json, re
import pandas as pd
from matplotlib import pyplot as plt
#read all the files and make all the graphs
#path_to_json = '.'
#json_files = [pos_json for pos_json in os.listdir(path_to_json) if pos_json.endswith('.json')]
#print(json_files)  # for me this prints ['foo.json']

def get_colors(strings):
    colors = []
    for s in strings:
        hash = 153163
        r = (hash & 0xFF0000) >> 16;
        g = (hash & 0x00FF00) >> 8;
        b = hash & 0x0000FF;
        colors.append('g-')

def pfs(postfix, strings):
    res = []
    for s in strings:
        res.append(s + postfix)
    return res

def createPlot(title, line_labels, files):
    yss = [[]]
    xss = [[]]
    for file in files:
        with open(file) as json_file:
            data = json.load(json_file)
            for program_value in data.values():
                for datasets in program_value.values():
                    xs = []
                    ys = []
                    for name, dataset in datasets.items():
                        length = 0
                        acc = 0
                        for time in dataset['runtimes']:
                            length += 1
                            acc += time
                        ys.append(acc/length)
                        xs.append(int(re.search('[0-9]+', name).group(0)))
                    xs.sort()
                    ys.sort()
                    xss.append(xs)
                    yss.append(ys)
    lines = []
    fig, ax1 = plt.subplots(figsize=(4,4))
    for i in range(len(line_labels)):
        plt.plot(xss[i+1], yss[i+1])

    fig.legend(bbox_to_anchor=(0.13, 0.87),
               labels=line_labels,
               loc="upper left",
               mode="fit")

    ax1.set_ylabel('Runetime (ms)')
    plt.title(title)

    plt.xlabel("InputSize")
    plt.xscale("log")
    plt.show()

def wrapper(targets, title, postfix):
    files = pfs(postfix, targets)
    createPlot(title, targets, files, False)

createPlot("matrix-inversion", ["moderate", "incremental", "autotuned"],["matrix-inversion-mod-opencl.json", "matrix-inversion-inc-opencl.json", "matrix-inversion-tuned-opencl.json"])
