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
    distance_match = re.search(r"The smallest distance is ([\d.]+)", text)
    distance = float(distance_match.group(1)) if distance_match else None

    points_match = re.findall(r"Point \{x = ([\d.-]+), y = ([\d.-]+)\}", text)
    closest_points = [(float(x), float(y)) for x, y in points_match[-2:]]

    generated_points = []
    match = re.search(r"Generated Points:\s*((?:[ \t]*-?\d+(?:\.\d+)?[ \t]+-?\d+(?:\.\d+)?\s*\n?)*)", text)
    if match:
        for line in match.group(1).strip().splitlines():
            try:
                x, y = map(float, line.strip().split())
                generated_points.append((x, y))
            except ValueError:
                pass

    return distance, closest_points, generated_points

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python closest_pair_draw.py <output_file.txt>")
        sys.exit(1)

    filename = sys.argv[1]
    encoding, _ = detect_encoding(filename)

    try:
        with open(filename, "r", encoding=encoding) as f:
            output = f.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)

    distance, closest_points, generated_points = parse_output(output)

    xs, ys = zip(*generated_points)
    plt.scatter(xs, ys, label="Generated Points", color="skyblue")

    cp_xs, cp_ys = zip(*closest_points)
    plt.plot(cp_xs, cp_ys, color="red", linewidth=2, label=f"Closest Pair ({distance:.2f})")
    plt.scatter(cp_xs, cp_ys, color="red")

    plt.title("Closest Pair of Points")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.legend()
    plt.grid(True)
    plt.axis('equal')
    plt.show()




