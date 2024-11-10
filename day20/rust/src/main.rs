use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
enum Module {
    Broadcaster(Vec<String>),
    FlipFlop(bool, Vec<String>),
    Conjunction(HashMap<String, bool>, Vec<String>),
}

impl Module {
    fn receivers(&self) -> &[String] {
        match self {
            Module::Broadcaster(receivers) => receivers,
            Module::FlipFlop(_, receivers) => receivers,
            Module::Conjunction(_, receivers) => receivers,
        }
    }
}

// Run with:
//   cargo run < ../input
fn main() {
    let mut modules: HashMap<String, Module> = HashMap::new();
    let mut parents: Vec<(String, String)> = Vec::new();
    std::io::stdin().lines().for_each(|line| {
        if let Ok(line) = line {
            let (sender, receivers) = line.split_once(" -> ").unwrap();
            let receivers: Vec<String> = receivers.split(", ").map(|s| s.to_string()).collect();
            if sender == "broadcaster" {
                let module = Module::Broadcaster(receivers.clone());
                modules.insert(sender.to_string(), module);
                for receiver in receivers {
                    parents.push((receiver, sender.to_string()));
                }
            } else if sender.starts_with("%") {
                let name = sender.chars().skip(1).collect::<String>();
                let module = Module::FlipFlop(false, receivers.clone());
                modules.insert(name.clone(), module);
                for receiver in receivers {
                    parents.push((receiver, name.to_string()));
                }
            } else if sender.starts_with("&") {
                let name = sender.chars().skip(1).collect::<String>();
                let module = Module::Conjunction(HashMap::new(), receivers.clone());
                modules.insert(name.clone(), module);
                for receiver in receivers {
                    parents.push((receiver, name.to_string()));
                }
            } else {
                unreachable!();
            }
        }
    });
    for (name, sender) in parents {
        if let Some(Module::Conjunction(state, _)) = modules.get_mut(&name) {
            state.insert(sender, false);
        }
    }

    println!("First answer: {}", first_answer(modules.clone()));
    println!("Second answer: {}", second_answer(modules.clone()));
}

fn first_answer(mut modules: HashMap<String, Module>) -> usize {
    let mut pulses = Vec::new();
    for _ in 0..1000 {
        let mut queue = VecDeque::from([(
            "button-module".to_string(),
            false,
            "broadcaster".to_string(),
        )]);
        while let Some((sender, pulse, receiver)) = queue.pop_front() {
            pulses.push(pulse);

            if let Some(module) = modules.get_mut(&receiver) {
                match module {
                    Module::Broadcaster(receivers) => {
                        for receiver in receivers {
                            queue.push_back(("broadcaster".to_string(), pulse, receiver.to_string()));
                        }
                    }
                    Module::FlipFlop(state, receivers) => {
                        if !pulse {
                            *state = !*state;
                            for next_receiver in receivers {
                                queue.push_back((
                                    receiver.to_string(),
                                    *state,
                                    next_receiver.to_string(),
                                ));
                            }
                        }
                    }
                    Module::Conjunction(state, receivers) => {
                        if let Some(s) = state.get_mut(&sender) {
                            *s = pulse;
                        }
                        let p = !state.values().all(|p| *p);
                        for next_receiver in receivers {
                            queue.push_back((receiver.to_string(), p, next_receiver.to_string()));
                        }
                    }
                }
            }
        }
    }
    let (low, high): (Vec<_>, Vec<_>) = pulses.into_iter().partition(|p| *p);
    low.len() * high.len()
}

fn second_answer(mut modules: HashMap<String, Module>) -> usize {
    if let Some((parent, _)) = modules
        .iter()
        .find(|(_, m)| m.receivers().contains(&"rx".to_string()))
    {
        let parent = parent.clone();
        let mut prevs: HashMap<String, Vec<usize>> = HashMap::new();
        for n in 1.. {
            let mut queue = VecDeque::from([(
                "button-module".to_string(),
                false,
                "broadcaster".to_string(),
            )]);
            while let Some((sender, pulse, receiver)) = queue.pop_front() {
                if receiver == parent {
                    if let Some(Module::Conjunction(state, _)) = modules.get(&parent) {
                        state
                            .iter()
                            .filter(|(_, p)| **p)
                            .map(|(x, _)| x)
                            .for_each(|x| {
                                let e = prevs.entry(x.clone()).or_insert(Vec::new());
                                e.push(n);
                                e.dedup();
                            });
                        if !prevs.is_empty() && prevs.values().all(|ns| ns.len() >= 3) {
                            let xs = prevs.values().map(|ns| ns[1] - ns[0]).collect::<Vec<_>>();
                            return lcms(&xs);
                        }
                    }
                }
                if let Some(module) = modules.get_mut(&receiver) {
                    match module {
                        Module::Broadcaster(receivers) => {
                            for receiver in receivers {
                                queue.push_back((
                                    "broadcaster".to_string(),
                                    pulse,
                                    receiver.to_string(),
                                ));
                            }
                        }
                        Module::FlipFlop(state, receivers) => {
                            if !pulse {
                                *state = !*state;
                                for next_receiver in receivers {
                                    queue.push_back((
                                        receiver.to_string(),
                                        *state,
                                        next_receiver.to_string(),
                                    ));
                                }
                            }
                        }
                        Module::Conjunction(state, receivers) => {
                            if let Some(s) = state.get_mut(&sender) {
                                *s = pulse;
                            }
                            let p = !state.values().all(|p| *p);
                            for next_receiver in receivers {
                                queue.push_back((receiver.to_string(), p, next_receiver.to_string()));
                            }
                        }
                    }
                }
            }
        }
    }
    unreachable!();
}

fn lcms(ns: &[usize]) -> usize {
    ns.into_iter().cloned().reduce(lcm).unwrap()
}

fn lcm(a: usize, b: usize) -> usize {
    b * (a / gcd(a, b))
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd(b, a % b)
}
