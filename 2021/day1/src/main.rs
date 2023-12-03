use std::fs::read_to_string;

fn read_lines(filename: &str) -> Vec<String> {
    read_to_string(filename)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

fn parse_simple(lines: Vec<String>) -> Vec<u32> {
    lines
        .iter()
        .map(|x| x.parse().unwrap_or(0))
        .collect::<Vec<_>>()
}

fn parse_as_sliding_window(lines: Vec<String>) -> Vec<u32> {
    (0..(lines.len() - 2))
        .map(|i| {
            lines[i].parse().unwrap_or(0)
                + lines[i + 1].parse().unwrap_or(0)
                + lines[i + 2].parse().unwrap_or(0)
        })
        .collect::<Vec<_>>()
}

fn solve(measurements: Vec<u32>) -> usize {
    measurements
        .iter()
        .zip(measurements.iter().skip(1))
        .filter(|(x, y)| x < y)
        .collect::<Vec<_>>()
        .len()
}

fn main() {
    let lines = read_lines("./input.txt");
    // let first = parse_simple(lines);
    let second = parse_as_sliding_window(lines);

    // let count = solve(first);
    let count = solve(second);

    println!("{}", count);
}
