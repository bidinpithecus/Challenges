use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn safe_or_unsafe(vec: Vec<i32>) -> u32 {
    if vec.len() <= 1 {
        return 1;
    }

    let is_ascending = vec.windows(2).all(|w| w[0] < w[1]);
    let is_descending = vec.windows(2).all(|w| w[0] > w[1]);
    let is_changing_allowed = vec
        .windows(2)
        .all(|w| 1 <= (w[0] - w[1]).abs() && (w[0] - w[1]).abs() <= 3);

    if (is_ascending || is_descending) && is_changing_allowed {
        return 1;
    }

    0
}

fn safe_or_unsafe_v2(vec: Vec<i32>) -> u32 {
    let len = vec.len();
    if safe_or_unsafe(vec.clone()) == 1 {
        return 1;
    }

    for i in 0..len {
        let mut vec_clone = vec.clone();
        vec_clone.remove(i);
        if safe_or_unsafe(vec_clone) == 1 {
            return 1;
        }
    }

    0
}

fn parse_file_fst_star(file: File) -> u32 {
    let mut safe_reports = 0;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.expect("Error parsing file");

        let numbers: Result<Vec<i32>, _> = line.split_whitespace().map(str::parse).collect();

        match numbers {
            Ok(vec) => safe_reports += safe_or_unsafe(vec),
            Err(e) => eprintln!("Error parsing numbers: {:?}", e),
        }
    }

    safe_reports
}

fn parse_file_snd_star(file: File) -> u32 {
    let mut safe_reports = 0;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.expect("Error parsing file");

        let numbers: Result<Vec<i32>, _> = line.split_whitespace().map(str::parse).collect();

        match numbers {
            Ok(vec) => safe_reports += safe_or_unsafe_v2(vec),
            Err(e) => eprintln!("Error parsing numbers: {:?}", e),
        }
    }

    safe_reports
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
            let safe_reports = parse_file_fst_star(file);

            println!("Number of safe reports: {}", safe_reports);
        }
        "2" => {
            let safe_reports = parse_file_snd_star(file);

            println!("Number of safe reports: {}", safe_reports);
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
