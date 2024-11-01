#[derive(Debug, Clone, Copy)]
struct Edge {
    start: Point,
    end: Point,
}

#[derive(Debug, Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
}

// Run with:
//   cargo run < ../input
fn main() {
    let input = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .collect::<Vec<String>>();
    let part1_edges = parse_part1(&input);
    println!("First answer: {}", filled_area(&part1_edges));
    let part2_edges = parse_part2(&input);
    println!("Second answer: {}", filled_area(&part2_edges));
}

fn parse_part1(input: &[String]) -> Vec<Edge> {
    input
        .iter()
        .scan(Point { x: 0, y: 0 }, |start, line| {
            let mut parts = line.split(' ');
            let direction = parts.next().unwrap();
            let steps = parts.next().unwrap().parse::<i64>().unwrap();
            parts.next(); // color
            let end = match direction {
                "U" => Point {
                    y: start.y - steps,
                    ..*start
                },
                "D" => Point {
                    y: start.y + steps,
                    ..*start
                },
                "L" => Point {
                    x: start.x - steps,
                    ..*start
                },
                "R" => Point {
                    x: start.x + steps,
                    ..*start
                },
                _ => unreachable!(),
            };
            let edge = Edge {
                start: *start,
                end: end,
            };
            *start = end;
            Some(edge)
        })
        .collect()
}

fn parse_part2(input: &[String]) -> Vec<Edge> {
    input
        .iter()
        .scan(Point { x: 0, y: 0 }, |start, line| {
            let mut parts = line.split(' ');
            parts.next(); // direction
            parts.next(); // steps
            let hex = parts.next().unwrap()
                .chars()
                .skip(2)
                .take(6)
                .collect::<String>();
            let (hex_steps, hex_direction) = hex.split_at(5);
            let steps = i64::from_str_radix(hex_steps, 16).unwrap();
            let direction = match hex_direction {
                "0" => "R",
                "1" => "D",
                "2" => "L",
                "3" => "U",
                _ => unreachable!(),
            };
            let end = match direction {
                "U" => Point {
                    y: start.y - steps,
                    ..*start
                },
                "D" => Point {
                    y: start.y + steps,
                    ..*start
                },
                "L" => Point {
                    x: start.x - steps,
                    ..*start
                },
                "R" => Point {
                    x: start.x + steps,
                    ..*start
                },
                _ => unreachable!(),
            };
            let edge = Edge {
                start: *start,
                end: end,
            };
            *start = end;
            Some(edge)
        })
        .collect()
}

fn filled_area(edges: &[Edge]) -> i64 {
    // shoelace formula
    let area: i64 = edges
        .iter()
        .map(|e| {
            let Point { x: x0, y: y0 } = e.start;
            let Point { x: x1, y: y1 } = e.end;
            y0 * x0 + y0 * (-x1) + y1 * x0 + y1 * (-x1)
        })
        .sum();
    let area = area / 2;

    let perimiter: i64 = edges
        .iter()
        .map(|e| {
            let Point { x: x0, y: y0 } = e.start;
            let Point { x: x1, y: y1 } = e.end;
            (x0 - x1).abs() + (y0 - y1).abs()
        })
        .sum();

    area + perimiter / 2 + 1
}
