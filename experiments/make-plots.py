import pandas as pd
import matplotlib.pyplot as plt
import glob
import os
import sys
# Define column names
column_names = ["node", "function", "module", "number", "calls", "time-individual", "mem-individual", "time-inherited", "mem-inherited"]

funclist = [" nodeLogic.processTXs.processTXs'", " validateTransaction", " nodeLogic.mint'"]

# Initialize an empty dataframe
df = pd.DataFrame(columns=column_names)

# make sure that pwd is "experiments"
if os.getcwd().split("/")[-1] != "experiments":
    print("Please run this script from the experiments directory")
    sys.exit(1)

# cd to the "profiled_outputs" directory
os.chdir("profiled_outputs")

for directory in os.listdir():
    # cd to the directory
    os.chdir(directory)
    for capacity in os.listdir():
        # cd to the capacity directory
        os.chdir(capacity)
        df = pd.DataFrame(columns=column_names)
        for file_name in glob.glob("calls.txt"):
            # Read the file into a dataframe
            file_df = pd.read_csv(file_name, names=column_names, skiprows=0)
            # keep only the last /<string> for the "node" column
            file_df["node"] = file_df["node"].str.split("/").str[-1]
            # and remove the .prof.prof suffix
            file_df["node"] = file_df["node"].str.replace(".prof.prof:", "", regex=True)
            # Append the file dataframe to the main dataframe
            # use pandas.concat to append the dataframe to the main dataframe
            df = pd.concat([df, file_df], ignore_index=True)

        # Pivot the dataframe to make "function" into columns and "execution_time" into values
        pivoted = df.pivot(index="node", columns="function", values="time-inherited")

        # make node1 appear first and node10 last
        pivoted = pivoted.reindex(sorted(pivoted.index, key=lambda x: int(x.split("node")[1])))

        # Plot a bar plot for each function
        pivoted.plot(kind="bar", rot=30, figsize=(12, 7))

        # Add a legend
        plt.legend()
        # horizontal grid lines
        plt.grid(axis="y")
        plt.xlabel("Node")
        plt.ylabel("Percentage of total execution time")
        plt.title(f"Execution time per function per node ({capacity})")
        plt.savefig(f"times_of_function_per_node_{capacity}.png")
        # Show the plot
        # plt.show()
        plt.close()

        # cd back to the capacity directory
        os.chdir("..")
    os.chdir("..")
os.chdir("..")