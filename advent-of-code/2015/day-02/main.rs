use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
struct RectangularCuboid {
    l: u8,
    w: u8,
    h: u8,
}

impl RectangularCuboid {
    fn new(l: u8, w: u8, h: u8) -> Self {
        RectangularCuboid { l, w, h }
    }

    fn surface_area(&self) -> u32 {
        let area1 = self.l as u32 * self.w as u32;
        let area2 = self.w as u32 * self.h as u32;
        let area3 = self.h as u32 * self.l as u32;

        2 * (area1 + area2 + area3)
    }

    fn smallest_area(&self) -> u32 {
        let area1 = self.l as u32 * self.w as u32;
        let area2 = self.w as u32 * self.h as u32;
        let area3 = self.h as u32 * self.l as u32;

        area1.min(area2).min(area3)
    }

    fn smallest_perimeter(&self) -> u32 {
        let perimeter1 = 2 * (self.l as u32 + self.w as u32);
        let perimeter2 = 2 * (self.w as u32 + self.h as u32)
        let perimeter3 = 2 * (self.h as u32 + self.l as u32);

        perimeter1.min(perimeter2).min(perimeter3)
    }

    fn volume(&self) -> u32 {
        (self.l as u32 * self.w as u32 * self.h as u32) as u32
    }
}

fn parse_file(file_path: &str) -> io::Result<Vec<RectangularCuboid>> {
    let file = File::open(file_path)?;
    let reader = io::BufReader::new(file);
    let mut nums = Vec::new();

    for (line_number, line) in reader.lines().enumerate() {
        let line = line?;
        let nums_in_line: Vec<u8> = line
            .split('x')
            .map(|num_str| num_str.trim().parse::<u8>().unwrap())
            .collect();

        if nums_in_line.len() == 3 {
            let cuboid = RectangularCuboid::new(nums_in_line[0], nums_in_line[1], nums_in_line[2]);
            nums.push(cuboid);
        } else {
            eprintln!("Invalid format in line {}: {:?}", line_number + 1, line);
        }
    }

    Ok(nums)
}

fn main() {
    let file_path = "input.txt";
    let mut amount_of_square_feet = 0;
    let mut amount_of_ribbon = 0;

    match parse_file(file_path) {
        Ok(vec) => {
            for cuboid in vec {
                amount_of_square_feet += cuboid.surface_area() + cuboid.smallest_area();
                amount_of_ribbon += cuboid.smallest_perimeter() + cuboid.volume()
            }
            println!("Amount of square feet of paper: {}", amount_of_square_feet);
            println!("Amount of ribbon: {}", amount_of_ribbon);
        }
        Err(err) => eprintln!("Error reading file {}: {}", file_path, err),
    }
}
