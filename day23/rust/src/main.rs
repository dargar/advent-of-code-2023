use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let map: HashMap<(isize, isize), char> = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| *c != '#')
                .map(move |(x, c)| ((x as isize, y as isize), c))
        })
        .collect();

    println!("First answer: {}", first_answer(&map));
    println!("Second answer: {}", second_answer(&map));
}

fn first_answer(map: &HashMap<(isize, isize), char>) -> usize {
    let start = *map.keys().min_by_key(|(_, y)| y).unwrap();
    let goal = *map.keys().max_by_key(|(_, y)| y).unwrap();

    let mut paths = Vec::new();
    let mut frontier = vec![vec![start]];
    while let Some(path) = frontier.pop() {
        let mut it = path.iter().rev();
        let curr = *it.next().unwrap();
        let prev = *it.next().unwrap_or(&(-1, -1));

        if curr == goal {
            paths.push(path);
            continue;
        }

        let mut candidates = Vec::new();
        {
            let next = (curr.0, curr.1 - 1);
            if next != prev && map.get(&next).is_some_and(|c| *c == '.' || *c == '^') {
                let mut next_path = path.clone();
                next_path.push(next);
                candidates.push(next_path);
            }
        }
        {
            let next = (curr.0, curr.1 + 1);
            if next != prev && map.get(&next).is_some_and(|c| *c == '.' || *c == 'v') {
                let mut next_path = path.clone();
                next_path.push(next);
                candidates.push(next_path);
            }
        }
        {
            let next = (curr.0 - 1, curr.1);
            if next != prev && map.get(&next).is_some_and(|c| *c == '.' || *c == '<') {
                let mut next_path = path.clone();
                next_path.push(next);
                candidates.push(next_path);
            }
        }
        {
            let next = (curr.0 + 1, curr.1);
            if next != prev && map.get(&next).is_some_and(|c| *c == '.' || *c == '>') {
                let mut next_path = path.clone();
                next_path.push(next);
                candidates.push(next_path);
            }
        }
        frontier.extend(candidates);
    }

    paths.iter().map(|path| path.len()).max().unwrap() - 1
}

fn second_answer(map: &HashMap<(isize, isize), char>) -> u32 {
    let start = *map.keys().min_by_key(|(_, y)| y).unwrap();
    let goal = *map.keys().max_by_key(|(_, y)| y).unwrap();

    let mut pois: HashSet<(isize, isize)> = HashSet::new();
    let mut nodes: HashMap<((isize, isize), (isize, isize)), u32> = HashMap::new();
    {
        let mut stack: Vec<(isize, isize)> = vec![start, goal];
        while let Some(pos) = stack.pop() {
            if !pois.insert(pos) {
                continue;
            }
            let reachable = crossroads(map, pos);
            stack.extend(reachable.keys());
            nodes.extend(reachable.iter().map(|(p, s)| ((pos, *p), *s)));
            nodes.extend(reachable.iter().map(|(p, s)| ((*p, pos), *s)));
        }
    }

    let mut stack: Vec<Vec<(isize, isize)>> = vec![vec![start]];
    let mut answer = 0;
    while let Some(path) = stack.pop() {
        let tail = *path.last().unwrap();
        if tail == goal {
            let steps = path
                .windows(2)
                .map(|w| nodes.get(&(w[0], w[1])).unwrap())
                .sum();
            answer = std::cmp::max(answer, steps);
            continue;
        }

        stack.extend(
            pois.iter()
                .filter(|p| !path.contains(p))
                .map(|p| (tail, *p))
                .filter(|pair| nodes.contains_key(pair))
                .map(|(_, p)| {
                    let mut next_path = path.clone();
                    next_path.push(p);
                    next_path
                }),
        );
    }

    answer
}

fn crossroads(
    map: &HashMap<(isize, isize), char>,
    start: (isize, isize),
) -> HashMap<(isize, isize), u32> {
    let mut stack: Vec<((isize, isize), u32)> = vec![(start, 0)];
    let mut visited: HashSet<(isize, isize)> = HashSet::new();
    let mut crossroads: HashMap<(isize, isize), u32> = HashMap::new();
    while let Some((pos, steps)) = stack.pop() {
        if !visited.insert(pos) {
            continue;
        }

        let neighbours = neighbours(pos);
        if pos != start
            && neighbours
                .iter()
                .all(|n| *map.get(n).unwrap_or(&'#') != '.')
        {
            crossroads.insert(pos, steps);
        } else {
            stack.extend(
                neighbours
                    .into_iter()
                    .filter(|n| !visited.contains(n))
                    .filter(|n| *map.get(n).unwrap_or(&'#') != '#')
                    .map(|p| (p, steps + 1)),
            );
        }
    }
    crossroads
}

fn neighbours((x, y): (isize, isize)) -> Vec<(isize, isize)> {
    vec![(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
}
