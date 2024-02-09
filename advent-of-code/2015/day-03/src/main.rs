use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};

fn parse_file(file_path: &str) -> io::Result<usize> {
    let file = File::open(file_path)?;
    let reader = io::BufReader::new(file);

    let mut houses: HashSet<(i32, i32)> = HashSet::new();
    let mut x1: i32 = 0;
    let mut y1: i32 = 0;
    let mut x2: i32 = 0;
    let mut y2: i32 = 0;
    houses.insert((x1, y1));

    for line in reader.lines() {
        let line = line?;
        let mut chars = line.chars();
        while let (Some(ch1), Some(ch2)) = (chars.next(), chars.next()) {
            match ch1 {
                '<' => x1 -= 1,
                '>' => x1 += 1,
                '^' => y1 += 1,
                'v' => y1 -= 1,
                _ => eprintln!("Error parsing the file"),
            }
            match ch2 {
                '<' => x2 -= 1,
                '>' => x2 += 1,
                '^' => y2 += 1,
                'v' => y2 -= 1,
                _ => eprintln!("Error parsing the file"),
            }
            houses.insert((x1, y1));
            houses.insert((x2, y2));
        }
    }

    Ok(houses.len())
}

fn main() {
    let file_path = "input.txt";

    match parse_file(file_path) {
        Ok(num_of_houses) => {
            println!("Number of houses: {}", num_of_houses);
        }
        Err(err) => eprintln!("Error reading file {}: {}", file_path, err),
    }
}
