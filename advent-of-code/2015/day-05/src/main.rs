use fancy_regex::Regex;
use std::fs::File;
use std::io::{self, BufRead};

fn read_file(path: &str) -> io::Result<io::Lines<io::BufReader<File>>> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    Ok(reader.lines())
}

fn is_word_nice(word: &str) -> bool {
    let cannot_contain = ["ab", "cd", "pq", "xy"];
    let vowels = ['a', 'e', 'i', 'o', 'u'];
    let mut num_of_vowels: u8 = 0;

    for restriction in cannot_contain {
        if word.contains(restriction) {
            return false;
        }
    }

    for letter in word.chars() {
        if vowels.contains(&letter) {
            num_of_vowels += 1;
        }
    }

    if num_of_vowels < 3 {
        return false;
    }

    let mut chars = word.chars();
    if let Some(mut ch1) = chars.next() {
        while let Some(ch2) = chars.next() {
            if ch1 == ch2 {
                return true;
            }
            ch1 = ch2;
        }
    }

    return false;
}

fn is_word_nicer(word: &str) -> bool {
    let pattern1 = Regex::new(r"(\w\w)\w*\1").unwrap();
    let pattern2 = Regex::new(r"(\w).\1").unwrap();

    let match1 = pattern1.is_match(word);
    let match2 = pattern2.is_match(word);

    assert!(match1.is_ok());
    assert!(match2.is_ok());

    match1.unwrap() && match2.unwrap()
}

fn get_nice_words_count(file_path: &str) -> u32 {
    let lines = match read_file(file_path) {
        Ok(lines) => lines,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file_path, err);
            return 0;
        }
    };

    let mut num_of_nice_words = 0;

    for line in lines {
        if let Ok(word) = line {
            if is_word_nice(&word) {
                num_of_nice_words += 1;
            }
        } else {
            eprintln!("Error reading line from file {}", file_path);
        }
    }

    num_of_nice_words
}

fn get_nicer_words_count(file_path: &str) -> u32 {
    let lines = match read_file(file_path) {
        Ok(lines) => lines,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file_path, err);
            return 0;
        }
    };

    let mut num_of_nice_words = 0;

    for line in lines {
        if let Ok(word) = line {
            if is_word_nicer(&word) {
                num_of_nice_words += 1;
            }
        } else {
            eprintln!("Error reading line from file {}", file_path);
        }
    }

    num_of_nice_words
}

fn main() {
    let file_path = "input.txt";

    println!("There are {} nice words!", get_nice_words_count(file_path));
    println!("There are {} nicer words!", get_nicer_words_count(file_path));
}
