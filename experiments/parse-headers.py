import pandas as pd
import matplotlib.pyplot as plt
import os


def find_in(lines, pattern):
    for i, line in enumerate(lines):
        if line.startswith(pattern):
            return i
    return -1


def parse_header(filename):
    with open(filename, 'r') as f:
        contents = f.read()

    contents = contents.split('\n')
    start = find_in(contents, 'COST')
    end = find_in(contents[start+1:], 'COST')
    header = contents[start:start+end-1]
    header = [line for line in header if line]
    centers = [line.split()[0] for line in header[1:]]
    modules = [line.split()[1] for line in header[1:]]
    times = [line.split()[2] for line in header[1:]]
    mems = [line.split()[3] for line in header[1:]]
    # return a dictionary where the keys are the centers
    # and the values are (modules, times, mems)
    zipped = zip(centers, modules, times, mems)
    dictionary = {center: (mod, t, mem) for center, mod, t, mem in zipped}
    return dictionary


if __name__ == '__main__':
    DIRECTORIES = ["throughput_outdir_profiled", "scalability_outdir_profiled",
                   "throughput_outdir_profiled_txdelayed",
                   "scalability_outdir_profiled_txdelayed",
                   "throughput_outdir_profiled_txdelayed_less",
                   "scalability_outdir_profiled_txdelayed_less"]
    for direc in DIRECTORIES:
        subdirs = os.listdir(direc)
        for filedir in subdirs:
            if not filedir.startswith("capacity"):
                continue
            upper = 6 if direc.startswith("throughput") else 11
            FILENAMES = [f"{direc}/{filedir}/node{i}.prof.prof" for i in range(1, upper)]
            dicts = [parse_header(filename) for filename in FILENAMES]
            # keep the keys that are common to all dictionaries
            all_keys = [set(d.keys()) for d in dicts]
            common_keys = set.intersection(*all_keys)
            dictionary = {key: [] for key in common_keys}
            for d in dicts:
                for key in common_keys:
                    dictionary[key].append(float(d[key][2]))
            # for each list of times, get the average
            avgs = {key: sum(times) / len(times)
                    for key, times in dictionary.items()}
            avgs = {k: v for k, v in sorted(avgs.items(), key=lambda i: i[1])}
            # sort the dictionary by the average time
            dictionary = {k: v for k, v in zip(avgs.keys(), [dictionary[key] for key in avgs.keys()])}
            df = pd.DataFrame(dictionary, index=[f"node{i}" for i in range(1, upper)])
            df.plot(kind='bar', alpha=0.7, rot=30, figsize=(12, 7))
            plt.title("Top common cost centers")
            plt.xlabel("Node ID")
            plt.ylabel("Percentage of total execution time")
            plt.grid()
            plt.savefig(f'{direc}/{filedir}/cost-centers-{filedir}.png')
            plt.close()

            df = pd.DataFrame.from_dict(avgs, orient='index')
            df.plot(kind='bar', alpha=0.7, rot=5, figsize=(12, 7))
            # remove legend
            plt.legend().remove()
            plt.title("Average of cost centers across all nodes")
            plt.xlabel("Cost center")
            plt.ylabel("Percentage of total execution time")
            plt.grid()
            plt.savefig(f'{direc}/{filedir}/average-cost-centers-{filedir}.png')
            plt.close()
