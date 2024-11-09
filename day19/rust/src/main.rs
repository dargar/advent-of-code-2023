use std::collections::HashMap;
use std::io::Read;

// Run with:
//   cargo run < ../input
fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let (raw_workflows, raw_ratings) = input.split_once("\n\n").unwrap();
    let workflows = raw_workflows
        .lines()
        .map(|line| {
            let (name, rest) = line.split_once("{").unwrap();
            let (cond, _) = rest.split_once("}").unwrap();
            (name.to_string(), cond.to_string())
        })
        .collect::<HashMap<_, _>>();
    let ratings = raw_ratings
        .lines()
        .map(|line| {
            let (_, rest) = line.split_once("{").unwrap();
            let (values, _) = rest.split_once("}").unwrap();
            values
                .split(",")
                .map(|variable| {
                    variable
                        .chars()
                        .rev()
                        .enumerate()
                        .flat_map(|(i, c)| c.to_digit(10).map(|d| (i, d)))
                        .map(|(i, d)| 10u32.pow(i as u32) * d)
                        .sum::<u32>()
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let first_answer = ratings
        .iter()
        .filter(|rating| is_accepted(&workflows, rating))
        .flatten()
        .sum::<u32>();
    println!("First answer: {}", first_answer);
    println!("Second answer: {}", distinct_combinations(&workflows));
}

fn is_accepted(workflows: &HashMap<String, String>, rating: &[u32]) -> bool {
    let mut stack = vec!["in".to_string()];
    while let Some(name) = stack.pop() {
        if let Some(workflow) = workflows.get(&name) {
            let mut buf: Vec<char> = Vec::new();
            let mut vars: Vec<String> = Vec::new();
            let mut ops = Vec::new();
            for c in workflow.chars() {
                if c == '<' || c == '>' {
                    let var = buf.drain(..).collect();
                    vars.push(var);
                    ops.push(c);
                } else if c == ':' {
                    let var = buf.drain(..).collect();
                    vars.push(var);
                } else if c == ',' {
                    let dest = buf.drain(..).collect::<String>();
                    let threshold = vars.pop().unwrap().parse::<u32>().unwrap();
                    let var = match vars.pop().as_deref() {
                        Some("x") => rating[0],
                        Some("m") => rating[1],
                        Some("a") => rating[2],
                        Some("s") => rating[3],
                        _ => unreachable!(),
                    };
                    let cond = match ops.pop() {
                        Some('<') => var < threshold,
                        Some('>') => var > threshold,
                        _ => unreachable!(),
                    };
                    if cond {
                        match dest.as_ref() {
                            "R" => {
                                return false;
                            }
                            "A" => {
                                return true;
                            }
                            _ => {
                                stack.push(dest);
                                break;
                            }
                        }
                    }
                } else {
                    buf.push(c);
                }
            }
            if !buf.is_empty() {
                let dest = buf.drain(..).collect::<String>();
                match dest.as_ref() {
                    "R" => {
                        return false;
                    }
                    "A" => {
                        return true;
                    }
                    _ => {
                        stack.push(dest);
                    }
                }
            }
        }
    }
    unreachable!();
}

fn distinct_combinations(workflows: &HashMap<String, String>) -> u64 {
    let mut stack = vec![("in".to_string(), vec![(1, 4000); 4])];
    let mut accepted_ratings = Vec::new();
    while let Some((name, mut rating)) = stack.pop() {
        if name == "A" {
            accepted_ratings.push(rating);
            continue;
        }

        if let Some(workflow) = workflows.get(&name) {
            let mut buf: Vec<char> = Vec::new();
            let mut vars: Vec<String> = Vec::new();
            let mut ops = Vec::new();
            for c in workflow.chars() {
                if c == '<' || c == '>' {
                    let var = buf.drain(..).collect();
                    vars.push(var);
                    ops.push(c);
                } else if c == ':' {
                    let var = buf.drain(..).collect();
                    vars.push(var);
                } else if c == ',' {
                    let dest = buf.drain(..).collect::<String>();
                    let threshold = vars.pop().unwrap().parse::<u32>().unwrap();
                    let idx = match vars.pop().as_deref() {
                        Some("x") => 0,
                        Some("m") => 1,
                        Some("a") => 2,
                        Some("s") => 3,
                        _ => unreachable!(),
                    };
                    let (lo, hi) = rating[idx];
                    let (yes, no) = match ops.pop().unwrap() {
                        '<' => (
                            (lo, std::cmp::min(hi, threshold - 1)),
                            (std::cmp::max(lo, threshold), hi),
                        ),
                        '>' => (
                            (std::cmp::max(lo, threshold + 1), hi),
                            (lo, std::cmp::min(hi, threshold)),
                        ),
                        _ => unreachable!(),
                    };
                    let mut dest_rating = rating.clone();
                    dest_rating[idx] = yes;
                    stack.push((dest, dest_rating));

                    rating[idx] = no;
                } else {
                    buf.push(c);
                }
            }
            if !buf.is_empty() {
                let dest = buf.drain(..).collect::<String>();
                stack.push((dest, rating));
            }
        }
    }
    accepted_ratings
        .into_iter()
        .map(|rating| {
            rating
                .into_iter()
                .map(|(lo, hi)| (hi - lo) as u64 + 1)
                .product::<u64>()
        })
        .sum()
}
