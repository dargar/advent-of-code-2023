use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let input = fs::read_to_string("../input")?;
    let mut parts = input.split("\n\n");
    let seeds = parts
        .next()
        .unwrap()
        .split_whitespace()
        .skip(1)
        .map(|s| s.parse().unwrap())
        .collect::<Vec<u64>>();
    let mappings = parts
        .map(|part| {
            part.lines()
                .skip(1)
                .map(|line| {
                    line.split_whitespace()
                        .map(|s| s.parse().unwrap())
                        .collect::<Vec<u64>>()
                })
                .collect::<Vec<Vec<u64>>>()
        })
        .collect::<Vec<_>>();
    let first_answer = seeds
        .iter()
        .cloned()
        .map(|seed| {
            mappings.iter().fold(seed, |source_number, ranges| {
                ranges
                    .iter()
                    .filter(|conversion| {
                        let _destination_range_start = conversion[0];
                        let source_range_start = conversion[1];
                        let range_length = conversion[2];
                        source_range_start <= source_number
                            && source_number < source_range_start + range_length
                    })
                    .map(|conversion| {
                        let destination_range_start = conversion[0];
                        let source_range_start = conversion[1];
                        let _range_length = conversion[2];
                        let offset = source_number - source_range_start;
                        destination_range_start + offset
                    })
                    .next()
                    .unwrap_or(source_number)
            })
        })
        .min()
        .unwrap();
    println!("First answer: {}", first_answer);

    let second_answer = seeds
        .chunks(2)
        .flat_map(|seeds| {
            mappings.iter().fold(
                vec![(seeds[0], seeds[0] + seeds[1])],
                |source_numbers, ranges| {
                    let mut unknown = source_numbers;
                    let mut known = Vec::new();
                    while let Some((s0, s1)) = unknown.pop() {
                        let mut found = false;
                        for conversion in ranges {
                            let destination_range_start = conversion[0];
                            let source_range_start = conversion[1];
                            let range_length = conversion[2];
                            let src0 = source_range_start;
                            let src1 = src0 + range_length;
                            let dst0 = destination_range_start;
                            let dst1 = dst0 + range_length;
                            if s1 < src0 {
                                continue;
                            }
                            if s0 > src1 {
                                continue;
                            }
                            if s0 < src0 && s1 > src1 {
                                unknown.push((s0, src0 - 1));
                                known.push((dst0, dst1));
                                unknown.push((src1 + 1, s1));
                                found = true;
                                break;
                            } else if src0 <= s0 && s1 <= src1 {
                                let offset = s0 - src0;
                                let length = s1 - s0;
                                let dst_start = dst0 + offset;
                                let dst_end = dst_start + length;
                                known.push((dst_start, dst_end));
                                found = true;
                                break;
                            } else if src0 <= s1 && s1 <= src1 {
                                unknown.push((s0, src0 - 1));
                                known.push((dst0, dst0 + (s1 - src0)));
                                found = true;
                                break;
                            } else if src0 <= s0 && s0 <= src1 {
                                let offset = s0 - src0;
                                known.push((dst0 + offset, dst1));
                                unknown.push((src1 + 1, s1));
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            known.push((s0, s1));
                        }
                    }
                    known
                },
            )
        })
        .map(|(a, _)| a)
        .min()
        .unwrap();
    println!("Second answer: {}", second_answer);
    Ok(())
}
