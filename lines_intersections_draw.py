import sys
import re
import matplotlib.pyplot as plt

def parse_output(text):
    segments = []
    intersections = []

    segment_section = re.search(r"Generated Lines:\s*((?:.+?->.+?\n)+)", text)
    if segment_section:
        for line in segment_section.group(1).strip().splitlines():
            match = re.findall(r'[-+]?\d*\.\d+|\d+', line)
            if len(match) == 4:
                x1, y1, x2, y2 = map(float, match)
                segments.append([(x1, y1), (x2, y2)])

    intersections += [
        (float(x), float(y))
        for x, y in re.findall(r'at \(\s*([-\d.]+)\s*,\s*([-\d.]+)\s*\)', text)
    ]

    count_match = re.search(r'Number of intersections:\s*(\d+)', text)
    intersection_count = int(count_match.group(1)) if count_match else len(intersections)

    return segments, intersections, intersection_count


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python intersections_draw.py <output_file.txt>")
        sys.exit(1)

    filename = sys.argv[1]

    try:
        with open(filename, "r", encoding='utf-16le') as f:
            output = f.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)

    segments, intersections, count = parse_output(output)

    print(f"Segments: {segments}")
    print(f"Intersections ({count}): {intersections}")

    fig, ax = plt.subplots()

    for i, ((x1, y1), (x2, y2)) in enumerate(segments):
        ax.plot([x1, x2], [y1, y2], label=f"Segment {i+1}")

    if intersections:
        x_vals, y_vals = zip(*intersections)
        ax.scatter(x_vals, y_vals, color='red', s=80, label='Intersections', zorder=5)

        for x, y in intersections:
            ax.annotate(f'({x:.1f}, {y:.1f})', (x, y), textcoords="offset points", xytext=(5,5), ha='center', fontsize=8)

    ax.set_aspect('equal')
    ax.set_title(f"Line Segments and {count} Intersection(s)")
    ax.grid(True)
    ax.legend()
    plt.show()

