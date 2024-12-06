use std::io;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn crosswords(path: String) -> io::Result<Vec<Vec<char>>> {
    let file = File::open(&path)?;
    let reader = BufReader::new(file);

    let mut lines_as_chars = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let chars: Vec<char> = line.chars().collect();
        lines_as_chars.push(chars);
    }

    Ok(lines_as_chars)
}

fn parse_file_fst_star(path: String) -> u32 {
    let mut num_of_words = 0;
    let crosswords = crosswords(path).expect("Error reading file");
    let rows = crosswords.len();
    let cols = crosswords[0].len();

    // Horizontally
    for line in &crosswords {
        for window in line.windows(4) {
            if window == ['X', 'M', 'A', 'S'] || window == ['S', 'A', 'M', 'X'] {
                num_of_words += 1;
            }
        }
    }

    // Vertically
    if !&crosswords.is_empty() {
        for col in 0..cols {
            let mut column_chars: Vec<char> = Vec::new();
            for row in &crosswords {
                if let Some(&ch) = row.get(col) {
                    column_chars.push(ch);
                }
            }
            for window in column_chars.windows(4) {
                if window == ['X', 'M', 'A', 'S'] || window == ['S', 'A', 'M', 'X'] {
                    num_of_words += 1;
                }
            }
        }
    }

    // Diagonally (Top left to Bottom Right)
    for diag_start in 0..(rows + cols - 1) {
        let mut diagonal_chars: Vec<char> = Vec::new();
        for row in 0..rows {
            let col = diag_start as isize - row as isize;
            if col >= 0 && col < cols as isize {
                diagonal_chars.push(crosswords[row][col as usize]);
            }
        }
        for window in diagonal_chars.windows(4) {
            if window == ['X', 'M', 'A', 'S'] || window == ['S', 'A', 'M', 'X'] {
                num_of_words += 1;
            }
        }
    }

    // Diagonally (Top right to Bottom left)
    for diag_start in 0..(rows + cols - 1) {
        let mut diagonal_chars: Vec<char> = Vec::new();
        for row in 0..rows {
            let col = (cols as isize - 1 - diag_start as isize) + row as isize;
            if col >= 0 && col < cols as isize {
                diagonal_chars.push(crosswords[row][col as usize]);
            }
        }
        for window in diagonal_chars.windows(4) {
            if window == ['X', 'M', 'A', 'S'] || window == ['S', 'A', 'M', 'X'] {
                num_of_words += 1;
            }
        }
    }
    num_of_words
}

fn parse_file_snd_star(path: String) -> u32 {
    let mut num_of_words = 0;
    let crosswords = crosswords(path).expect("Error reading file");

    for (i, line) in crosswords.iter().enumerate() {
        for (j, window_0) in line.windows(3).enumerate() {
            if i + 2 < crosswords.len() {
                let window_1 = &crosswords[i + 1][j..j + 3];
                let window_2 = &crosswords[i + 2][j..j + 3];
                let left_to_right = &format!("{}{}{}", window_0[0], window_1[1], window_2[2]);
                let right_to_left = &format!("{}{}{}", window_0[2], window_1[1], window_2[0]);
                if (left_to_right == "MAS" || left_to_right == "SAM")
                    && (right_to_left == "MAS" || right_to_left == "SAM")
                {
                    num_of_words += 1;
                }
            }
        }
    }

    num_of_words
}

fn main() -> std::io::Result<()> {
    let star = std::env::args().nth(1).expect("no star number given");
    let path = std::env::args().nth(2).expect("no path given");

    match star.as_str() {
        "1" => {
            let answer = parse_file_fst_star(path);
            println!("Answer: {}", answer);
        }
        "2" => {
            let answer = parse_file_snd_star(path);
            println!("Answer: {}", answer);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_input_fst_star() {
        let num_of_words = parse_file_fst_star("input/example-1-star-1.txt".to_string());

        assert_eq!(num_of_words, 4)
    }

    #[test]
    fn second_input_fst_star() {
        let num_of_words = parse_file_fst_star("input/example-2-star-1.txt".to_string());

        assert_eq!(num_of_words, 18)
    }

    #[test]
    fn first_input_snd_star() {
        let num_of_words = parse_file_snd_star("input/example-1-star-2.txt".to_string());

        assert_eq!(num_of_words, 1)
    }

    #[test]
    fn second_input_snd_star() {
        let num_of_words = parse_file_snd_star("input/example-2-star-2.txt".to_string());

        assert_eq!(num_of_words, 9)
    }
}
