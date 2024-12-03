use std::collections::BTreeMap;
use std::fs::File;
use std::io::{self, BufRead};

fn parse_file_fst_star(file: File) -> (Vec<u32>, Vec<u32>) {
    let mut vec1: Vec<u32> = vec![];
    let mut vec2: Vec<u32> = vec![];
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line.expect("Error parsing file");
        let mut split = line.split_whitespace();

        let mut parsed_num = split
            .next()
            .expect("Error parsing first column")
            .parse::<u32>()
            .expect("Error converting first column");

        vec1.push(parsed_num);

        parsed_num = split
            .next()
            .expect("Error parsing second column")
            .parse::<u32>()
            .expect("Error converting second column");

        vec2.push(parsed_num);
    }

    vec1.sort();
    vec2.sort();

    (vec1, vec2)
}

fn parse_file_snd_star(file: File) -> (Vec<u32>, BTreeMap<u32, u32>) {
    let mut vec1: Vec<u32> = vec![];
    let mut map: BTreeMap<u32, u32> = BTreeMap::new();
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line.expect("Error parsing file");
        let mut split = line.split_whitespace();

        let mut parsed_num = split
            .next()
            .expect("Error parsing first column")
            .parse::<u32>()
            .expect("Error converting first column");

        vec1.push(parsed_num);

        parsed_num = split
            .next()
            .expect("Error parsing first column")
            .parse::<u32>()
            .expect("Error converting first column");

        *map.entry(parsed_num).or_insert(0) += 1;
    }

    vec1.sort();

    (vec1, map)
}

fn vec_diff(vec1: Vec<u32>, vec2: Vec<u32>) -> u32 {
    assert_eq!(vec1.len(), vec2.len());

    let mut diff = 0;

    for (num1, num2) in vec1.iter().zip(vec2.iter()) {
        diff += num1.abs_diff(*num2);
    }

    diff
}

fn lists_similarity(vec1: Vec<u32>, map: BTreeMap<u32, u32>) -> u32 {
    let mut sim = 0;

    for num1 in vec1 {
        sim += num1 * map.get(&num1).unwrap_or(&0);
    }

    sim
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
            let (id_list_1, id_list_2) = parse_file_fst_star(file);

            let diff = vec_diff(id_list_1, id_list_2);

            println!("Total distance between the lists: {}", diff);
        }
        "2" => {
            let (id_list_1, id_list_2) = parse_file_snd_star(file);

            let sim = lists_similarity(id_list_1, id_list_2);

            println!("Total similarity between the lists: {}", sim);
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
