import argparse
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection


parser = argparse.ArgumentParser()
parser.add_argument("file", type=str, metavar="FILE", help="spaceship problem file")
parser.add_argument("--output", "-o", type=str, metavar="FILE", required=True, help="output png file")
args = parser.parse_args()

targets = []
with open(args.file) as f:
    for line in f:
        x, y = line.split()
        targets.append((int(x), int(y)))

plt.gca().set_aspect('equal')
plt.gca().add_collection(LineCollection(zip([(0,0)] + targets, targets)))
plt.scatter([x for (x, y) in targets], [y for (x, y) in targets], label="target")
plt.scatter([0], [0], label="start")
plt.legend()
plt.savefig(args.output)
plt.clf()
