import sys
import re
import matplotlib.pyplot as plt

def parse_output(text):
    generated_points = []
    gen_match = re.search(r"Generated Points:\s*((?:\s*-?\d+(?:\.\d+)?\s+-?\d+(?:\.\d+)?\s*\n?)*)", text)
    if gen_match:
        for line in gen_match.group(1).strip().splitlines():
            try:
                x, y = map(float, line.strip().split())
                generated_points.append((x, y))
            except ValueError:
                continue

    hull_points = []
    hull_match = re.search(r"Convex Hull:\s*((?:\s*-?\d+(?:\.\d+)?\s+-?\d+(?:\.\d+)?\s*\n?)*)", text)
    if hull_match:
        for line in hull_match.group(1).strip().splitlines():
            try:
                x, y = map(float, line.strip().split())
                hull_points.append((x, y))
            except ValueError:
                continue

    return generated_points, hull_points

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Error")
        sys.exit(1)

    filename = sys.argv[1]

    try:
        with open(filename, 'r', encoding='utf-16le') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"File '{filename}' not found.")
        sys.exit(1)

    points, convex_hull = parse_output(content)

    if not points:
        print("No points found.")
        sys.exit(1)

    px, py = zip(*points)
    hx, hy = zip(*convex_hull)

    if convex_hull and convex_hull[0] != convex_hull[-1]:
        hx += (hx[0],)
        hy += (hy[0],)

    plt.figure(figsize=(6, 6))
    plt.plot(px, py, 'o', label='Points', color='skyblue')
    if convex_hull:
        plt.plot(hx, hy, 'r-', lw=2, label='Convex Hull')
        plt.fill(hx, hy, 'r', alpha=0.2)

    plt.title("Convex Hull of Given Points")
    plt.legend()
    plt.grid(True)
    plt.axis('equal')
    plt.show()

