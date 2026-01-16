import csv
import json
import sys
import re

def convert_unit(value_ps):
    """Convert picoseconds to a more readable unit if necessary, or just return ns."""
    # tasty-bench outputs picoseconds. Let's convert to nanoseconds for the report
    return float(value_ps) / 1000.0

def main():
    if len(sys.argv) < 3:
        print("Usage: bench_to_json.py <input.csv> <output.json>")
        sys.exit(1)

    csv_file = sys.argv[1]
    json_file = sys.argv[2]
    
    results = []
    
    # Regex to handle CSV lines properly if quotes are involved, 
    # though standard csv module handles strict formats well.
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            # tasty-bench CSV columns: Name,Mean (ps),2*Stdev (ps)
            # Depending on flags, it might have memory stats too.
            
            name = row.get('Name', '').strip()
            if not name:
                continue
                
            # Parse Mean (ps)
            # The header is literally "Mean (ps)"
            mean_ps_str = row.get('Mean (ps)', '0')
            try:
                mean_ps = float(mean_ps_str)
            except ValueError:
                continue
            
            # Convert to ns
            value_ns = convert_unit(mean_ps)
            
            # Parse 2*Stdev (ps)
            stdev_ps_str = row.get('2*Stdev (ps)', '0')
            try:
                stdev_ps = float(stdev_ps_str)
                range_ns = convert_unit(stdev_ps)
            except ValueError:
                range_ns = 0

            # Construct entry for time
            entry = {
                "name": name,
                "unit": "ns",
                "value": value_ns,
                "range": range_ns
            }
            results.append(entry)

            # Handle Memory Stats if present
            # Columns: Allocated, Copied, Peak Memory
            if 'Allocated' in row:
                try:
                    allocated = float(row['Allocated'])
                    results.append({
                        "name": f"{name} - Allocated",
                        "unit": "Bytes",
                        "value": allocated
                    })
                except ValueError:
                    pass

            if 'Copied' in row:
                try:
                    copied = float(row['Copied'])
                    # Copied is usually small, maybe less interesting, but let's include it
                    results.append({
                        "name": f"{name} - Copied",
                        "unit": "Bytes",
                        "value": copied
                    })
                except ValueError:
                    pass

            if 'Peak Memory' in row:
                try:
                    peak = float(row['Peak Memory'])
                    results.append({
                        "name": f"{name} - Peak Memory",
                        "unit": "Bytes",
                        "value": peak
                    })
                except ValueError:
                    pass

    with open(json_file, 'w') as f:
        json.dump(results, f, indent=2)

if __name__ == "__main__":
    main()
