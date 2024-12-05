use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

use regex::Regex;

fn parse_file_fst_star(file: File) -> i32 {
    let mut total_sum = 0;
    let reader = BufReader::new(file);

    let regex = Regex::new(r"mul\((\d+),(\d+)\)").expect("Error parsing the string");
    for line in reader.lines() {
        let line = line.expect("Error parsing file");

        for occur in regex.captures_iter(&line) {
            if occur.len() != 3 {
                return 0;
            }
            let num_1: &i32 = &occur[1].parse().expect("Failed to parse first number");
            let num_2: &i32 = &occur[2].parse().expect("Failed to parse first number");

            total_sum += num_1 * num_2;
        }
    }

    total_sum
}

fn parse_file_snd_star(file: File) -> i32 {
    let mut total_sum = 0;
    let mut enabled = true;
    let reader = BufReader::new(file);

    let regex_do = Regex::new(r"do\(\)").expect("Error parsing the string");
    let regex_dont = Regex::new(r"don't\(\)").expect("Error parsing the string");
    let regex_mul = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").expect("Error parsing the string");

    let regex = Regex::new(&format!(r"{}|{}|{}", regex_do, regex_dont, regex_mul))
        .expect("Error parsing the regex");

    for line in reader.lines() {
        let line = line.expect("Error parsing file");

        for match_result in regex.find_iter(&line) {
            let matched = match_result.as_str();

            if regex_do.is_match(matched) {
                enabled = true;
            } else if regex_dont.is_match(matched) {
                enabled = false;
            } else if enabled {
                if let Some(occurs) = regex_mul.captures(matched) {
                    if occurs.len() != 3 {
                        return 0;
                    }
                    let num_1: i32 = occurs[1].parse().expect("Failed to parse number");
                    let num_2: i32 = occurs[2].parse().expect("Failed to parse number");

                    total_sum += num_1 * num_2;
                }
            }
        }
    }

    total_sum
}

fn main() -> std::io::Result<()> {
    let star = std::env::args().nth(1).expect("no star number given");
    let path = std::env::args().nth(2).expect("no path given");

    let file = match File::open(path) {
        Ok(file) => file,
        Err(error) => match error.kind() {
            std::io::ErrorKind::NotFound => {
                eprintln!("File not found");
                return Err(error);
            }
            _ => return Err(error),
        },
    };

    match star.as_str() {
        "1" => {
            let total_sum = parse_file_fst_star(file);

            println!("Answer: {}", total_sum);
        }
        "2" => {
            let total_sum = parse_file_snd_star(file);

            println!("Answer: {}", total_sum);
        }
        _ => {
            eprintln!("Invalid star number given");
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Invalid star number",
            ));
        }
    }
    Ok(())
}
