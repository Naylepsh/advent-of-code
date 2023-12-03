use std::fs::read_to_string;

fn read_lines(filename: &str) -> Vec<String> {
    read_to_string(filename)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

struct ReadingSummary {
    zeros: u32,
    ones: u32,
}

impl ReadingSummary {
    pub fn empty() -> ReadingSummary {
        ReadingSummary { zeros: 0, ones: 0 }
    }

    pub fn combine(left: ReadingSummary, right: ReadingSummary) -> ReadingSummary {
        ReadingSummary {
            zeros: left.zeros + right.zeros,
            ones: left.ones + right.ones,
        }
    }

    pub fn combine_mut(&mut self, other: ReadingSummary) {
        self.zeros += other.zeros;
        self.ones += other.ones;
    }
}

impl From<Option<char>> for ReadingSummary {
    fn from(value: Option<char>) -> Self {
        match value {
            Some('0') => ReadingSummary { zeros: 1, ones: 0 },
            Some('1') => ReadingSummary { zeros: 0, ones: 1 },
            _ => ReadingSummary::empty(),
        }
    }
}

impl From<char> for ReadingSummary {
    fn from(value: char) -> Self {
        match value {
            '0' => ReadingSummary { zeros: 1, ones: 0 },
            '1' => ReadingSummary { zeros: 0, ones: 1 },
            _ => ReadingSummary::empty(),
        }
    }
}

fn compute_complex_rating(
    lines: Vec<String>,
    i: usize,
    determine_bit: fn(u32, u32) -> char,
) -> u32 {
    if lines.len() == 1 {
        isize::from_str_radix(lines[0].as_str(), 2).unwrap() as u32
    } else {
        let summary = lines
            .iter()
            .map(|line| ReadingSummary::from(line.chars().nth(i)))
            .fold(ReadingSummary::empty(), ReadingSummary::combine);

        let bit_value = determine_bit(summary.zeros, summary.ones);

        let leftover = lines
            .into_iter()
            .filter(|line| line.chars().nth(i).unwrap() == bit_value)
            .collect::<Vec<_>>();

        compute_complex_rating(leftover, i + 1, determine_bit)
    }
}

fn determine_oxygen_bit(zeros: u32, ones: u32) -> char {
    if ones >= zeros {
        '1'
    } else {
        '0'
    }
}

fn determine_co2_bit(zeros: u32, ones: u32) -> char {
    if ones >= zeros {
        '0'
    } else {
        '1'
    }
}

fn solve_first(lines: Vec<String>) -> u32 {
    let length = lines.first().map(|line| line.len()).unwrap_or(0);
    let mut summaries = (0..length)
        .map(|_| ReadingSummary::empty())
        .collect::<Vec<_>>();

    lines.iter().for_each(|line| {
        line.chars().enumerate().for_each(|(i, bit)| {
            summaries[i].combine_mut(ReadingSummary::from(bit));
        })
    });

    let (gamma, epsilon) = summaries.iter().fold((0, 0), |(gamma, epsilon), summary| {
        if summary.ones > summary.zeros {
            ((gamma << 1) + 1, epsilon << 1)
        } else {
            (gamma << 1, (epsilon << 1) + 1)
        }
    });

    gamma * epsilon
}

fn solve_second(lines: Vec<String>) -> u32 {
    let oxygen = compute_complex_rating(lines.clone(), 0, determine_oxygen_bit);
    let co2 = compute_complex_rating(lines, 0, determine_co2_bit);

    println!("O:{} CO2:{}", oxygen, co2);

    oxygen * co2
}

fn main() {
    let lines = read_lines("./input.txt");
    // let lines = read_lines("./example.txt");

    println!("first: {}", solve_first(lines.clone()));
    println!("second: {}", solve_second(lines));
}
