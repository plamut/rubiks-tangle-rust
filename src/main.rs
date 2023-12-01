// TODO: learn how to use rust formatter... rustfmt Ctr+Shit+I
// TODO: integrate into VS code.. read docs for shortcuts:
// https://code.visualstudio.com/docs/languages/rust
//
// TODO: learn how to write proper docstrings, generate local docs

use rubiks_tangle::solve;
use rubiks_tangle::TerminalStyle::{BOLD, RESET};

fn main() {
    println!("Solving the Rubik's tangle puzzle...");

    let n_found: u32 = solve();

    println!("*** SOLUTIONS FOUND: {BOLD}{n_found}{RESET}");
}
