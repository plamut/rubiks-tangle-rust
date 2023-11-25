use std::collections::HashMap;

pub fn solve() -> u32 {
    let mut n_solutions = 0;
    let tiles = define_tiles();

    n_solutions
}

#[derive(Eq, PartialEq, Hash)]
enum TileSide {
    Front,
    Back,
}

enum Orientation {
    North,
    East,
    South,
    West,
}

#[derive(Clone, Copy)]
enum Color {
    Red,
    Blue,
    Green,
    Yellow,
}

use crate::Color::*;
use crate::Orientation::*;
use crate::TileSide::*;

// TODO: better: a tuple of four edges////
// TODO: how to force uniqueness? Wioth a struct constructor probably to make sure all exist?

// 2 colors per edge (first and second, in clockwise direction), four edges altogether.
// The first edge is a reference edge of the tile, and its color indices are 0 and 1,
// respectively.
// If we enumerate the tile colors in clockwise direction, the reference edge contains
// the colors (0, 1), the next edge the colors (2, 3) and so on.
//
//  0 1
// 7    2
// 6    3
//   5 4
//
// NOTE: When the indexes are used to enumerate the tile's back side, we pretend that we
// can "see through" the tile when placed face up (as oposed to an alternative where we
// would physically turn the tile face down and then enumerating the colors that we see)
type TileFace = [[Color; 2]; 4]; // 4 edgese, each with two colors

struct Tile {
    side_shown: TileSide,
    orientation: Orientation,            // orientation of the reference edge
    colors: HashMap<TileSide, TileFace>, // TODO: let it be a tuple... with exactly two items
}

impl Tile {
    fn new(front_colors: &[Color; 8], back_colors: &[Color; 8]) -> Tile {
        let mut colors = HashMap::new();

        colors.insert(
            Front,
            [
                [front_colors[0], front_colors[1]],
                [front_colors[2], front_colors[3]],
                [front_colors[4], front_colors[5]],
                [front_colors[6], front_colors[7]],
            ],
        );
        colors.insert(
            Back,
            [
                [back_colors[0], back_colors[1]],
                [back_colors[2], back_colors[3]],
                [back_colors[4], back_colors[5]],
                [back_colors[6], back_colors[7]],
            ],
        );

        Tile {
            side_shown: Front,
            orientation: North,
            colors: colors,
        }
    }
}

fn define_tiles() -> Vec<Tile> {
    let mut tiles: Vec<Tile> = Vec::new();

    let tile = Tile::new(
        &[Green, Blue, Red, Green, Blue, Yellow, Red, Yellow],
        &[Yellow, Blue, Red, Yellow, Blue, Green, Red, Green],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Green, Yellow, Blue, Red, Green, Blue, Red, Yellow],
        &[Yellow, Green, Red, Blue, Yellow, Blue, Green, Red],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Green, Red, Yellow, Blue, Green, Yellow, Blue, Red],
        &[Yellow, Red, Blue, Green, Yellow, Green, Red, Blue],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Blue, Yellow, Red, Green, Blue, Green, Yellow, Red],
        &[Green, Blue, Yellow, Red, Green, Yellow, Red, Blue],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Yellow, Red, Green, Blue, Yellow, Blue, Red, Green],
        &[Blue, Red, Yellow, Green, Blue, Yellow, Green, Red],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Blue, Red, Green, Yellow, Blue, Yellow, Red, Green],
        &[Red, Blue, Yellow, Green, Red, Yellow, Green, Blue],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Yellow, Green, Blue, Red, Yellow, Red, Green, Blue],
        &[Green, Yellow, Red, Blue, Green, Red, Blue, Yellow],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Red, Yellow, Blue, Yellow, Green, Red, Blue, Green],
        &[Red, Green, Yellow, Green, Blue, Red, Yellow, Blue],
    );
    tiles.push(tile);

    let tile = Tile::new(
        &[Green, Red, Yellow, Red, Blue, Green, Yellow, Blue],
        &[Green, Yellow, Red, Yellow, Blue, Green, Red, Blue],
    );
    tiles.push(tile);

    tiles
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
