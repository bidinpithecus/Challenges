use md5;

fn mine_advent_coins(input_str: &str) -> u64 {
    let mut num = 1;
    loop {
        let input = format!("{}{}", input_str, num);
        let digest = md5::compute(input);
        let digest_hex = format!("{:x}", digest);

        if digest_hex.starts_with("000000") {
            return num;
        }
        num += 1;
    }
}

fn main() {
    let input = "yzbqklnj";
    let answer = mine_advent_coins(input);

    println!("The lowest number is: {}", answer);
}
