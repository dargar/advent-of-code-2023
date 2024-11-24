use std::collections::HashMap;
use std::collections::HashSet;
use std::io::stdin;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;

    let mut bricks: HashMap<char, Vec<(i32, i32, i32)>> = input
        .lines()
        .map(|line| {
            let (lhs, rhs) = line.split_once("~").unwrap();
            let lhs: Vec<_> = lhs.split(",").map(|s| s.parse().unwrap()).collect();
            let rhs: Vec<_> = rhs.split(",").map(|s| s.parse().unwrap()).collect();
            if lhs[0] != rhs[0] {
                (lhs[0]..=rhs[0]).map(|x| (x, lhs[1], lhs[2])).collect()
            } else if lhs[1] != rhs[1] {
                (lhs[1]..=rhs[1]).map(|y| (lhs[0], y, lhs[2])).collect()
            } else if lhs[2] != rhs[2] {
                (lhs[2]..=rhs[2]).map(|z| (lhs[0], lhs[1], z)).collect()
            } else {
                vec![(lhs[0], lhs[1], lhs[2])]
            }
        })
        .zip('A'..)
        .map(|(brick, c)| (c, brick))
        .collect();

    run(&mut bricks);

    let mut first_answer = 0;
    let mut second_answer = 0;
    for c in ('A'..).take(bricks.len()) {
        let mut bs = bricks.clone();
        bs.remove(&c);
        let moved = run(&mut bs);
        first_answer += if moved.is_empty() { 1 } else { 0 };
        second_answer += moved.len();
    }

    println!("First answer: {}", first_answer);
    println!("Second answer: {}", second_answer);

    Ok(())
}

fn run(bricks: &mut HashMap<char, Vec<(i32, i32, i32)>>) -> HashSet<char> {
    let max_z = bricks.values().flatten().map(|(_, _, z)| *z).max().unwrap();

    let mut bricks_that_moved = HashSet::new();
    let mut try_move = true;
    while try_move {
        let mut moved = false;
        let mut cubes_in_previous_layer: HashSet<(i32, i32, i32)> = HashSet::new();
        for z in 1..=max_z {
            let bricks_at_this_layer: Vec<char> = bricks
                .iter()
                .filter(|(_, brick)| brick.iter().any(|(_, _, bz)| *bz == z))
                .map(|(c, _)| *c)
                .collect();

            let bricks_at_this_layer_that_can_move: Vec<char> = bricks_at_this_layer
                .into_iter()
                .filter(|c| {
                    let brick = bricks.get(c).unwrap();
                    if brick.iter().any(|(_, _, bz)| *bz == 1) {
                        return false;
                    }
                    brick
                        .iter()
                        .map(|(bx, by, bz)| (*bx, *by, *bz - 1))
                        .all(|c| !cubes_in_previous_layer.contains(&c))
                })
                .collect();

            for c in bricks_at_this_layer_that_can_move.iter() {
                let bs = bricks.get_mut(c).unwrap();
                for (_, _, cz) in bs {
                    *cz -= 1;
                }
            }

            cubes_in_previous_layer = bricks
                .values()
                .flatten()
                .filter(|(_, _, cz)| *cz == z)
                .cloned()
                .collect();

            moved = moved || !bricks_at_this_layer_that_can_move.is_empty();
            bricks_that_moved.extend(bricks_at_this_layer_that_can_move);
        }
        try_move = moved;
    }
    bricks_that_moved
}
