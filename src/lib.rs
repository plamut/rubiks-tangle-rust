use std::collections::HashMap;

pub fn solve() -> u32 {
    let tiles = define_tiles();
    let mut board = Board::new();
    let mut n_solutions = 0;

    for (i, &ref tile) in tiles.iter().enumerate() {
        board.place_tile(i, &tile);
    }

    board.pretty_print();

    n_solutions
}

struct Board<'a> {
    // These are the indexes of the board's slots:
    //   0 1 2
    //   3 4 5
    //   6 7 8
    slots: [Option<&'a Tile>; 9],
}

impl<'a> Board<'a> {
    fn new() -> Board<'a> {
        Board {
            slots: [None, None, None, None, None, None, None, None, None],
        }
    }

    fn place_tile(&mut self, slot_idx: usize, tile: &'a Tile) {
        match self.slots[slot_idx] {
            Some(_) => {
                panic!("Slot {} already taken", slot_idx);
            }
            None => {
                self.slots[slot_idx] = Some(tile);
            }
        }
    }

    fn pretty_print(&self) {
        println!();

        // TODO: use box formatting unicode characters... more pretty
        println!("┌─────┬─────┬─────╮");

        for row in 0..=2 {
            let tile0 = self.slots[3 * row];
            let tile1 = self.slots[3 * row + 1];
            let tile2 = self.slots[3 * row + 2];

            // TODO: better: get colors for all edges?
            // Or just switch to index-based representation?

            // first character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let (edge_colors, is_main) = Self::edge_colors_top(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            let (edge_colors, is_main) = Self::edge_colors_top(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            let (edge_colors, is_main) = Self::edge_colors_top(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            println!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            // second character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let (edge_colors, is_main) = Self::edge_colors_lr_upper(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let (edge_colors, is_main) = Self::edge_colors_lr_upper(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let (edge_colors, is_main) = Self::edge_colors_lr_upper(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            println!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            // third character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let (edge_colors, is_main) = Self::edge_colors_lr_lower(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let (edge_colors, is_main) = Self::edge_colors_lr_lower(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let (edge_colors, is_main) = Self::edge_colors_lr_lower(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            println!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            // fourth character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let (edge_colors, is_main) = Self::edge_colors_bottom(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            let (edge_colors, is_main) = Self::edge_colors_bottom(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            print!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            let (edge_colors, is_main) = Self::edge_colors_bottom(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors, is_main);
            println!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            if row < 2 {
                println!("├─────┼─────┼─────┤");
            } else {
                println!("╰─────┴─────┴─────╯");
            }
        }
        println!();
    }

    fn edge_colors_top(tile: Option<&Tile>) -> ([Color; 2], bool) {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&tile.side_shown];

                let top_edge_idx = match tile.orientation {
                    North => 0,
                    East => 3,
                    South => 2,
                    West => 1,
                };

                return (shown_edges[top_edge_idx], tile.orientation == North);
            }
            None => {
                return ([Color::Undefined, Color::Undefined], false);
            }
        }
    }

    fn edge_colors_bottom(tile: Option<&Tile>) -> ([Color; 2], bool) {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&tile.side_shown];

                let bottom_edge_idx = match tile.orientation {
                    North => 2,
                    East => 1,
                    South => 0,
                    West => 3,
                };

                return (shown_edges[bottom_edge_idx], tile.orientation == South);
            }
            None => {
                return ([Color::Undefined, Color::Undefined], false);
            }
        }
    }

    // TODO: need two bools! for each color, they are on separate edges
    // TODO: change to return colors for all at once? and idx based representation?
    fn edge_colors_lr_upper(tile: Option<&Tile>) -> ([Color; 2], bool) {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&tile.side_shown];

                let left_edge_idx = match tile.orientation {
                    North => 3,
                    East => 2,
                    South => 1,
                    West => 0,
                };
                let right_edge_idx = match tile.orientation {
                    North => 1,
                    East => 0,
                    South => 3,
                    West => 2,
                };

                return (
                    [
                        shown_edges[left_edge_idx][1],
                        shown_edges[right_edge_idx][0],
                    ],
                    tile.orientation == West,
                );
            }
            None => {
                return ([Color::Undefined, Color::Undefined], false);
            }
        }
    }

    // TODO: need two bools! for each color, they are on separate edges
    // TODO: change to return colors for all at once? and idx based representation?
    fn edge_colors_lr_lower(tile: Option<&Tile>) -> ([Color; 2], bool) {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&tile.side_shown];

                let left_edge_idx = match tile.orientation {
                    North => 3,
                    East => 2,
                    South => 1,
                    West => 0,
                };
                let right_edge_idx = match tile.orientation {
                    North => 1,
                    East => 0,
                    South => 3,
                    West => 2,
                };

                return (
                    [
                        shown_edges[left_edge_idx][0],
                        shown_edges[right_edge_idx][1],
                    ],
                    tile.orientation == West,
                );
            }
            None => {
                return ([Color::Undefined, Color::Undefined], false);
            }
        }
    }

    fn to_terminal_chars(colors: &[Color; 2], main_edge: bool) -> [String; 2] {
        let mut result = [String::from(""), String::from("")];

        for (i, color) in colors.iter().enumerate() {
            let to_print: String;

            // TODO: have color codes in constants!
            if main_edge {
                to_print = match color {
                    Undefined => String::from("\x1b[7m \x1b[0m"),
                    Red => String::from("\x1b[7m\x1b[31mR\x1b[0m"),
                    Blue => String::from("\x1b[7m\x1b[34mB\x1b[0m"),
                    Green => String::from("\x1b[7m\x1b[32mG\x1b[0m"),
                    Yellow => String::from("\x1b[7m\x1b[33mY\x1b[0m"),
                };
            } else {
                to_print = match color {
                    Undefined => String::from(" "),
                    Red => String::from("\x1b[31mR\x1b[0m"),
                    Blue => String::from("\x1b[34mB\x1b[0m"),
                    Green => String::from("\x1b[32mG\x1b[0m"),
                    Yellow => String::from("\x1b[33mY\x1b[0m"),
                };
            }

            result[i] = to_print;
        }

        result
    }
}

#[derive(Eq, PartialEq, Hash)]
enum TileSide {
    Front,
    Back,
}

#[derive(PartialEq)]
enum Orientation {
    North,
    East,
    South,
    West,
}

#[derive(Clone, Copy, Debug)]
enum Color {
    Red,
    Blue,
    Green,
    Yellow,
    Undefined, // for when we need to return a Color, but there is none (e.g. missing tile)
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
    pub side_shown: TileSide,
    pub orientation: Orientation, // orientation of the reference edge
    pub colors: HashMap<TileSide, TileFace>, // TODO: let it be a tuple... with exactly two items
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
