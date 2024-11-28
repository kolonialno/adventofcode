import pandas as pd

df = pd.read_csv("02_data.csv")

# Part 1
#%%
horizontal_position = 0
depth = 0

for i in range(len(df)):
    if df.iloc[i].command == "forward":
        horizontal_position += df.iloc[i].movement
    elif df.iloc[i].command == "down":
        depth += df.iloc[i].movement
    elif df.iloc[i].command == "up":
        depth -= df.iloc[i].movement

print(depth*horizontal_position)


#Part 2
#%%
horizontal_position = 0
depth = 0
aim = 0

for i in range(len(df)):
    if df.iloc[i].command == "forward":
        horizontal_position += df.iloc[i].movement
        depth += aim*df.iloc[i].movement
    elif df.iloc[i].command == "down":
        aim += df.iloc[i].movement
    elif df.iloc[i].command == "up":
        aim -= df.iloc[i].movement

print(depth*horizontal_position)