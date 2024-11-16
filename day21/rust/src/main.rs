use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::io::stdin;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;

    let map: HashMap<(i64, i64), char> = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(move |(x, c)| ((x as i64, y as i64), c))
        })
        .collect();

    println!("First answer: {}", first_answer(&map));
    println!("Second answer: {}", second_answer(&map));

    Ok(())
}

fn first_answer(map: &HashMap<(i64, i64), char>) -> usize {
    let mut heads: HashSet<(i64, i64)> = map
        .iter()
        .filter(|(_, c)| **c == 'S')
        .map(|(pos, _)| *pos)
        .collect();
    for _ in 0..64 {
        heads = heads
            .drain()
            .flat_map(nsew)
            .filter(|pos| map.get(pos).is_some_and(|c| *c != '#'))
            .collect();
    }
    heads.len()
}

// Credit to https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
// for this solution... I was stuck for ages trying to find cycles in the number of steps per
// iteration.
fn second_answer(map: &HashMap<(i64, i64), char>) -> usize {
    let width = 1 + map.keys().map(|(x, _)| *x).max().unwrap();
    let height = 1 + map.keys().map(|(_, y)| *y).max().unwrap();
    assert_eq!(width, height);
    let size = width as usize;

    let mut frontier: VecDeque<(usize, (i64, i64))> = map
        .iter()
        .filter(|(_, c)| **c == 'S')
        .map(|(pos, _)| (0, *pos))
        .collect();
    let mut distances: HashMap<(i64, i64), usize> = HashMap::new();
    while let Some((distance, position)) = frontier.pop_front() {
        if distances.contains_key(&position) {
            continue;
        }
        distances.insert(position, distance);
        frontier.extend(
            nsew(position)
                .into_iter()
                .filter(|p| !distances.contains_key(p) && map.get(p).is_some_and(|c| *c != '#'))
                .map(|p| (distance + 1, p)),
        );
    }

    let n = (26501365 - size / 2) / size;

    let num_even = n * n;
    let num_odd = (n + 1) * (n + 1);

    let (evens, odds): (Vec<&usize>, Vec<&usize>) = distances.values().partition(|v| *v % 2 == 0);
    let num_even_corners = evens.iter().filter(|v| ***v > 65).count();
    let num_odd_corners = odds.iter().filter(|v| ***v > 65).count();

    num_odd * odds.len() + num_even * evens.len() - ((n + 1) * num_odd_corners)
        + (n * num_even_corners)
}

fn nsew((x, y): (i64, i64)) -> Vec<(i64, i64)> {
    vec![(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]
}
