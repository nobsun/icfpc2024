from pathlib import Path
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection

table = {
    "7": (-1, 1),
    "8": (0, 1),
    "9": (1, 1),
    "4": (-1, 0),
    "5": (0, 0),
    "6": (1, 0),
    "1": (-1, -1),
    "2": (0, -1),
    "3": (1, -1),
}


def simulate(s):
    pos = (0, 0)
    velocity = (0, 0)
    path = [(0, 0)]

    for c in s:
        dv = table[c]
        velocity = (velocity[0] + dv[0], velocity[1] + dv[1])
        pos = (pos[0] + velocity[0], pos[1] + velocity[1])
        path.append(pos)

    return path


for i in range(1, 26):
    prob_id = f"spaceship{i}"

    if not (Path("answers") / prob_id).exists():
        continue

    targets = []
    with open(Path("answers") / prob_id) as f:
        for line in f:
            x, y = line.split()
            targets.append((int(x), int(y)))

    if not (Path("solutions") / prob_id / "sol1.txt").exists():
        continue
    s = (Path("solutions") / prob_id / "sol1.txt").read_text()

    trajectories = simulate(s)
    lines = zip(trajectories, trajectories[1:])

    plt.scatter([x for (x, y) in trajectories], [y for (x, y) in trajectories])
    plt.gca().add_collection(LineCollection(zip(trajectories, trajectories[1:])))
    plt.scatter([x for (x, y) in targets], [y for (x, y) in targets], label="target")
    plt.legend()
    plt.savefig((Path("solutions") / prob_id / "sol1.png"))
    plt.clf()
