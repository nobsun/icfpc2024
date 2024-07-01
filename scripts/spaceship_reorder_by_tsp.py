# This requires GA-EAX-restart is installed as ./GA-EAX-restart/bin/GA-EAX-restart
# https://github.com/senshineL/GA-EAX-restart
import argparse
import math
import subprocess
from pathlib import Path


def dist(p1, p2):
    return math.sqrt((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2)


def reorder(points):
    points = [(0, 0)] + points

    tsp_filename = "spaceship_tsp.tmp.tsp"
    log_filename = "spaceship_tsp.tmp.out"

    with open(tsp_filename, "w") as f:
        print(f"DIMENSION : {len(points)}", file=f)
        print(f"EDGE_WEIGHT_TYPE : EUC_2D", file=f)
        print(f"NODE_COORD_SECTION", file=f)
        for i, p in enumerate(points):
            print(f"{i+1} {p[0]} {p[1]}", file=f)
        print(f"END", file=f)

    npop = 100
    nch = 30
    optimum = -1
    tmax = 10
    seed = 12345
    result = subprocess.run(
        [
            "./GA-EAX-restart/bin/GA-EAX-restart",
            tsp_filename,
            str(npop),
            str(nch),
            str(optimum),
            str(tmax),
            str(seed),
        ],
        capture_output=True,
        text=True,
    )

    with open("log_filename", "w") as f:
        print(result.stdout, file=f)

    lines = result.stdout.splitlines()
    i = 0
    for i, line in enumerate(lines):
        if line.startswith("bestval ="):
            break

    tour = [int(x) - 1 for x in lines[i + 2].split()]
    start_index = tour.index(0)
    tour = tour[start_index:] + tour[:start_index]
    assert tour[0] == 0

    if dist(points[0], points[tour[1]]) <= dist(points[0], points[tour[-1]]):
        tour = tour[1:]
    else:
        tour = list(reversed(tour[1:]))

    return [points[i] for i in tour]


parser = argparse.ArgumentParser()
parser.add_argument("file", type=str, metavar="FILE", help="spaceship problem file")
parser.add_argument(
    "--output", "-o", type=str, metavar="FILE", required=True, help="output png file"
)
args = parser.parse_args()

points = []
with open(args.file) as f:
    for line in f:
        x, y = line.split()
        points.append((int(x), int(y)))

points2 = reorder(points)
with open(args.output, "w") as f:
    for p in points2:
        print(f"{p[0]} {p[1]}", file=f)
