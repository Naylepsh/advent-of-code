use std::fs::read_to_string;

fn read_lines(filename: &str) -> Vec<String> {
    read_to_string(filename)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

fn parse_command(command: &String) -> (i32, i32) {
    let split = command.split_whitespace().collect::<Vec<_>>();
    match split.len() {
        2 => {
            let unit = split[1].parse().unwrap_or(0);
            match split[0] {
                "forward" => (unit, 0),
                "down" => (0, unit),
                "up" => (0, -unit),
                _ => (0, 0),
            }
        }
        _ => (0, 0),
    }
}

fn solve_first(commands: Vec<(i32, i32)>) -> i32 {
    let (x, y) = commands.iter().fold((0, 0), |(x, y), (delta_x, delta_y)| {
        (x + delta_x, y + delta_y)
    });
    x * y
}

fn solve_second(commands: Vec<(i32, i32)>) -> i32 {
    let (_, x, y) = commands
        .into_iter()
        .fold((0, 0, 0), |(aim, x, y), (delta_x, delta_aim)| {
            if delta_aim != 0 {
                (aim + delta_aim, x, y)
            } else {
                (aim, x + delta_x, y + aim * delta_x)
            }
        });
    x * y
}

fn main() {
    let lines = read_lines("./input.txt");
    let commands = lines.iter().map(parse_command).collect::<Vec<_>>();
    // let result = solve_first(commands);
    let result = solve_second(commands);
    // let (x, y) = lines.iter().map(parse_command).fold((0, 0), solve_first);
    // let result = x * y;
    println!("{}", result);
}
