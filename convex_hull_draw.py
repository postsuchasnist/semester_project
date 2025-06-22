import sys
import re
import matplotlib.pyplot as plt
import chardet

def detect_encoding(filepath, num_bytes=10000):
    with open(filepath, 'rb') as f:
        raw_data = f.read(num_bytes)
    result = chardet.detect(raw_data)
    return result['encoding'], result['confidence']

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
        print("Usage: python convex_hull_draw.py <output_file.txt>")
        sys.exit(1)

    filename = sys.argv[1]
    encoding, _ = detect_encoding(filename)

    try:
        with open(filename, 'r', encoding=encoding) as f:
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

