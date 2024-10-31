use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::io;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Crucible {
    heat_loss: u32,
    position: Position,
    direction: Direction,
    kind: CrucibleKind,
}

impl Crucible {
    fn new(direction: Direction, kind: CrucibleKind) -> Self {
        Self {
            heat_loss: 0,
            position: Position { x: 0, y: 0 },
            direction,
            kind,
        }
    }

    fn next(&self, heat_losses: &HashMap<Position, u32>) -> Vec<Crucible> {
        let mut result = Vec::new();
        match self.direction {
            Direction::North => {
                result.extend(self.steps(heat_losses, Direction::West));
                result.extend(self.steps(heat_losses, Direction::East));
            }
            Direction::East => {
                result.extend(self.steps(heat_losses, Direction::North));
                result.extend(self.steps(heat_losses, Direction::South));
            }
            Direction::South => {
                result.extend(self.steps(heat_losses, Direction::East));
                result.extend(self.steps(heat_losses, Direction::West));
            }
            Direction::West => {
                result.extend(self.steps(heat_losses, Direction::South));
                result.extend(self.steps(heat_losses, Direction::North));
            }
        }
        result
    }

    fn steps(
        &self,
        heat_losses: &HashMap<Position, u32>,
        direction: Direction,
    ) -> Vec<Crucible> {
        let (min_steps, max_steps) = match self.kind {
            CrucibleKind::Standard => (1, 3),
            CrucibleKind::Ultra => (4, 10),
        };
        (1..=max_steps)
            .map(|i| self.position.step(direction, i))
            .scan(self.heat_loss, |accumulated_heat_loss, position| {
                heat_losses.get(&position).map(|next_heat_loss| {
                    *accumulated_heat_loss += next_heat_loss;
                    (*accumulated_heat_loss, position)
                })
            })
            .skip(min_steps - 1)
            .map(|(next_heat_loss, next_position)| Crucible {
                heat_loss: next_heat_loss,
                position: next_position,
                direction,
                kind: self.kind,
            })
            .collect()
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug, PartialOrd, Ord)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    fn step(&self, direction: Direction, steps: isize) -> Position {
        match direction {
            Direction::North => Position {
                y: self.y - steps,
                ..*self
            },
            Direction::East => Position {
                x: self.x + steps,
                ..*self
            },
            Direction::South => Position {
                y: self.y + steps,
                ..*self
            },
            Direction::West => Position {
                x: self.x - steps,
                ..*self
            },
        }
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug, PartialOrd, Ord)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum CrucibleKind {
    Standard,
    Ultra,
}

// Run with:
//   cargo run < ../input
fn main() {
    let heat_losses: HashMap<Position, u32> = io::stdin()
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.unwrap()
                .chars()
                .enumerate()
                .map(move |(x, c)| {
                    (
                        Position {
                            x: x as isize,
                            y: y as isize,
                        },
                        c.to_digit(10).unwrap(),
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect();
    println!(
        "First answer: {}",
        answer(&heat_losses, CrucibleKind::Standard)
    );
    println!(
        "Second answer: {}",
        answer(&heat_losses, CrucibleKind::Ultra)
    );
}

fn answer(heat_losses: &HashMap<Position, u32>, crucible_kind: CrucibleKind) -> u32 {
    let destination = *heat_losses.keys().max().unwrap();

    let mut best_heat_losses: HashMap<(Position, Direction), u32> = HashMap::new();

    let mut priority_queue = BinaryHeap::new();
    priority_queue.push(Reverse(Crucible::new(Direction::South, crucible_kind)));
    priority_queue.push(Reverse(Crucible::new(Direction::East, crucible_kind)));

    while let Some(Reverse(crucible)) = priority_queue.pop() {
        if crucible.position == destination {
            return crucible.heat_loss;
        }

        if let Some(existing) = best_heat_losses.get(&(crucible.position, crucible.direction)) {
            if crucible.heat_loss > *existing {
                continue;
            }
        }

        for next_crucible in crucible.next(heat_losses) {
            if next_crucible.heat_loss
                < *best_heat_losses
                    .get(&(next_crucible.position, next_crucible.direction))
                    .unwrap_or(&u32::MAX)
            {
                best_heat_losses.insert(
                    (next_crucible.position, next_crucible.direction),
                    next_crucible.heat_loss,
                );
                priority_queue.push(Reverse(next_crucible));
            }
        }
    }
    unreachable!()
}
