//! # Rubik's Tangle
//!
//! `rubiks_tangle` contains a solver for the 3x3 double-sided Rubik's tangle puzzle.
//!
//! See the following link to get an idea what the puzzle is about:
//! <https://www.jaapsch.net/puzzles/tangle.htm>
//!

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

static mut COMBOS_TRIED: u32 = 0; // Yes, I'm still an overly optimistic n00b :)

/// A collection of terminal escape sequences for changing the appearance of the text.
#[allow(non_snake_case)]
pub mod TerminalStyle {
    pub const RESET: &str = "\x1b[0m";
    pub const BOLD: &str = "\x1b[1m";
    pub const INV: &str = "\x1b[7m";
    pub const RED: &str = "\x1b[31m";
    pub const BLUE: &str = "\x1b[34m";
    pub const GREEN: &str = "\x1b[32m";
    pub const YELLOW: &str = "\x1b[33m";
}

/// Solves the 3x3 Rubik's tangle puzzle and returns the number of solutions found.
///
/// NOTE: The function does not attempt to remove duplicates, i.e. solutions that are
/// essentially identical to each other, but merely rotated on the board.
pub fn solve() -> u32 {
    use TerminalStyle::{BOLD, RESET};

    let mut board = Board::new();

    #[allow(clippy::mutable_key_type)] // Tile's hash does not use mutable fields
    let unplaced_tiles = define_tiles();

    let n_solutions = find_solutions(&mut board, &unplaced_tiles);

    println!("*** COMBINATIONS CHECKED: {BOLD}{}{RESET}", unsafe {
        COMBOS_TRIED
    });

    n_solutions
}

/// Recursively tries to find the solution given a `Board` with some placed tiles and
/// a set of tiles that are yet to be placed.
///
/// Returns the umber of solutions found.
///
/// # Arguments
///
/// * `board` - A board with maybe some tiles already placed on it.
/// * `unplaced_tiles` - A set of remaining tiles not yet placed on the board.
///
#[allow(clippy::mutable_key_type)] // Tile's hash does not use mutable fields
fn find_solutions(board: &mut Board, unplaced_tiles: &HashSet<Rc<Tile>>) -> u32 {
    let mut n_solutions = 0;
    let mut tiles_to_check = unplaced_tiles.clone();

    for tile_ref in unplaced_tiles.iter() {
        let tile = Rc::clone(tile_ref);

        for side in [TileSide::Front, TileSide::Back].iter() {
            tile.set_side_shown(side);

            for orientation in [North, East, South, West].iter() {
                tile.set_orientation(orientation);

                unsafe { COMBOS_TRIED += 1 };
                if !board.can_place(&tile) {
                    continue;
                } else {
                    board.place_tile(Rc::clone(&tile));
                    tiles_to_check.remove(&tile);
                }

                if tiles_to_check.is_empty() {
                    n_solutions += 1;
                    println!("Found a solution!");
                    board.pretty_print();
                } else {
                    n_solutions += find_solutions(board, &tiles_to_check);
                }

                board.remove_tile();
                tiles_to_check.insert(Rc::clone(&tile));
            }
        }
    }

    n_solutions
}

/// Creates a set of unique game tiles with predefined colors on each side.
///
/// This function initializes and returns a `HashSet`` containing `Rc<Tile>` objects,
/// each representing a reference of one of the tiles in the game
///
/// # Returns
///
/// A set of pointers to the tiles used in the game.
///
#[allow(clippy::mutable_key_type)] // Tile's hash does not use mutable fields
fn define_tiles() -> HashSet<Rc<Tile>> {
    let mut tiles = HashSet::new();

    let mut sequence = 1..;

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Green, Blue, Red, Green, Blue, Yellow, Red, Yellow],
        &[Yellow, Blue, Red, Yellow, Blue, Green, Red, Green],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Green, Yellow, Blue, Red, Green, Blue, Red, Yellow],
        &[Yellow, Green, Red, Blue, Yellow, Blue, Green, Red],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Green, Red, Yellow, Blue, Green, Yellow, Blue, Red],
        &[Yellow, Red, Blue, Green, Yellow, Green, Red, Blue],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Blue, Yellow, Red, Green, Blue, Green, Yellow, Red],
        &[Green, Blue, Yellow, Red, Green, Yellow, Red, Blue],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Yellow, Red, Green, Blue, Yellow, Blue, Red, Green],
        &[Blue, Red, Yellow, Green, Blue, Yellow, Green, Red],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Blue, Red, Green, Yellow, Blue, Yellow, Red, Green],
        &[Red, Blue, Yellow, Green, Red, Yellow, Green, Blue],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Yellow, Green, Blue, Red, Yellow, Red, Green, Blue],
        &[Green, Yellow, Red, Blue, Green, Red, Blue, Yellow],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Red, Yellow, Blue, Yellow, Green, Red, Blue, Green],
        &[Red, Green, Yellow, Green, Blue, Red, Yellow, Blue],
    );
    tiles.insert(Rc::new(tile));

    let tile = Tile::new(
        sequence.next().unwrap(),
        &[Green, Red, Yellow, Red, Blue, Green, Yellow, Blue],
        &[Green, Yellow, Red, Yellow, Blue, Green, Red, Blue],
    );
    tiles.insert(Rc::new(tile));

    tiles
}

#[derive(Clone, Copy, Hash, PartialEq)]
enum Color {
    Red,
    Blue,
    Green,
    Yellow,
    Undefined, // for when we need to return a Color, but there is none (e.g. missing tile)
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum TileSide {
    Front,
    Back,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Orientation {
    North,
    East,
    South,
    West,
}

enum RelPlacement {
    Top,
    Right,
    Bottom,
    Left,
}

use crate::Color::*;
use crate::Orientation::*;
use crate::TileSide::*;

type TileFace = [[Color; 2]; 4]; // 4 edgese, each with two colors

/// A representation of a tile in the game.
///
/// Each tile has a front and the back side, and on each side it has 2 colored rope ends
/// on each of the four edges.
///
/// The `Tile` holds an identifier, a list of rope end colors on each of its edges, and
/// the information about the tile's placement - which side is face up and what the
/// orientation is with respect to the reference edge. The edges are enumerated in a
/// clockwise direction:
///
/// ````
///  0 0
/// 3   1
/// 3   1
///  2 2
///```
/// `
struct Tile {
    pub id: u32,
    pub side_shown: RefCell<TileSide>,
    pub orientation: RefCell<Orientation>, // orientation of the reference edge
    pub colors: HashMap<TileSide, TileFace>,
}

use std::hash::{Hash, Hasher};

impl Hash for Tile {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for edge in &self.colors[&Front] {
            edge[0].hash(state);
            edge[1].hash(state);
        }
        for edge in &self.colors[&Back] {
            edge[0].hash(state);
            edge[1].hash(state);
        }
    }
}
impl PartialEq for Tile {
    fn eq(&self, other: &Self) -> bool {
        self.colors[&Front] == other.colors[&Front] && self.colors[&Back] == other.colors[&Back]
    }
}
impl Eq for Tile {}

impl Tile {
    fn new(id: u32, front_colors: &[Color; 8], back_colors: &[Color; 8]) -> Tile {
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
            id,
            side_shown: RefCell::new(Front),
            orientation: RefCell::new(North),
            colors,
        }
    }

    fn set_side_shown(&self, new_side: &TileSide) {
        *self.side_shown.borrow_mut() = *new_side;
    }

    fn set_orientation(&self, new_orientation: &Orientation) {
        *self.orientation.borrow_mut() = *new_orientation;
    }

    /// Returns the pair of rope colors on the given edge while considering the tile's
    /// current orientation.
    ///
    /// The colors returned are ordered as if we were traversing along the tile's edges
    /// in the clockwise direction.
    fn edge_colors(&self, side: &RelPlacement) -> [Color; 2] {
        use Orientation::*;
        use RelPlacement::*;

        let edge_idx = match (*self.orientation.borrow(), side) {
            (North, Top) => 0,
            (North, Right) => 1,
            (North, Bottom) => 2,
            (North, Left) => 3,
            (East, Top) => 3,
            (East, Right) => 0,
            (East, Bottom) => 1,
            (East, Left) => 2,
            (South, Top) => 2,
            (South, Right) => 3,
            (South, Bottom) => 0,
            (South, Left) => 1,
            (West, Top) => 1,
            (West, Right) => 2,
            (West, Bottom) => 3,
            (West, Left) => 0,
        };
        let shown_edges = self.colors[&self.side_shown.borrow()];

        shown_edges[edge_idx]
    }

    /// Checks if another tile can be placed next to one of the tile's edges while not
    /// violating the edge colors (the rope colors on the adjacent edge need to match).
    fn can_adjoin(&self, other_tile: &Tile, side: &RelPlacement) -> bool {
        let our_colors = self.edge_colors(side);

        let opposite_side = match side {
            RelPlacement::Top => RelPlacement::Bottom,
            RelPlacement::Right => RelPlacement::Left,
            RelPlacement::Bottom => RelPlacement::Top,
            RelPlacement::Left => RelPlacement::Right,
        };
        let others_colors = other_tile.edge_colors(&opposite_side);

        // Edges of two adjacent tiles have opposite orientations, i.e. the color
        // indexes are reversed.
        our_colors[0] == others_colors[1] && our_colors[1] == others_colors[0]
    }
}

/// Representation of the 3x3 playing board.
///
/// Board's slots are enumerated frop lefto right, top to bottom starting in the top left
/// corner as shown below:
/// ````
///  0 1 2
///  3 4 5
///  6 7 8
/// ````
struct Board {
    slots: Vec<Option<Rc<Tile>>>,
    pub first_empty_idx: usize,
}

impl Board {
    fn new() -> Board {
        Board {
            slots: [None, None, None, None, None, None, None, None, None].to_vec(),
            first_empty_idx: 0,
        }
    }

    // indexes of all neighbor slots for each slot
    const ADJACENCIES: [[Option<usize>; 4]; 9] = [
        // North, East, South, West
        [None, Some(1), Some(3), None],
        [None, Some(2), Some(4), Some(0)],
        [None, None, Some(5), Some(1)],
        [Some(0), Some(4), Some(6), None],
        [Some(1), Some(5), Some(7), Some(3)],
        [Some(2), None, Some(8), Some(4)],
        [Some(3), Some(7), None, None],
        [Some(4), Some(8), None, Some(6)],
        [Some(5), None, None, Some(7)],
    ];

    /// Returns `true` if a tile can be placed on the first free slot while matching the
    /// rope colors on the edges of existing neighboring tiles on the board.
    fn can_place(&self, tile: &Tile) -> bool {
        let slot_idx = self.first_empty_idx;
        let adjacent_indexes = Self::ADJACENCIES[slot_idx];

        // TODO: rename, variables a bit for clarity
        let to_check = [
            // (item index, rel placement of new tile)
            (0, RelPlacement::Bottom),
            (1, RelPlacement::Left),
            (2, RelPlacement::Top),
            (3, RelPlacement::Right),
        ];

        for (item_idx, rel_placement) in to_check.iter() {
            let adjacent_slot_idx = adjacent_indexes[*item_idx];

            if let Some(slot_idx) = adjacent_slot_idx {
                let tile_in_slot: &Option<Rc<Tile>> = &self.slots[slot_idx];
                if let Some(existing_tile) = tile_in_slot {
                    if !existing_tile.can_adjoin(tile, rel_placement) {
                        return false;
                    }
                }
            }
        }

        true
    }

    /// Places a tile on the first free slot on the board.
    ///
    /// NOTE: The method assumes there is still at least one free slot on the board and
    /// that a tile can be placed on it without violating the rules of the game. Use
    /// the `can_place(...)` method for verifying that precondition before actually
    /// placing a tile.
    fn place_tile(&mut self, tile: Rc<Tile>) {
        match self.slots[self.first_empty_idx] {
            Some(_) => {
                panic!("Slot {} already taken", self.first_empty_idx);
            }
            None => {
                self.slots[self.first_empty_idx] = Some(Rc::clone(&tile));
                self.first_empty_idx += 1;
            }
        }
    }

    /// Removes a tile from the board currently placed at the highest slot index.
    ///
    /// NOTE: The method assumes there is at least one tile placed on the board already.
    fn remove_tile(&mut self) {
        match self.slots[self.first_empty_idx - 1] {
            Some(_) => {
                self.slots[self.first_empty_idx - 1] = None;
                self.first_empty_idx -= 1;
            }
            None => {
                panic!("Slot {} is already empty", self.first_empty_idx);
            }
        }
    }

    /// A helpefr fucntion for returning the colors of a tile on a aprticular edge
    /// and whether a color lives on the tile's reference edge.
    fn colors_info(tile: &Option<Rc<Tile>>, side: &RelPlacement) -> [(Color, bool); 2] {
        use Orientation::*;
        use RelPlacement::*;

        match tile {
            None => [(Color::Undefined, false), (Color::Undefined, false)],
            Some(tile) => {
                let edge = tile.edge_colors(side);
                let is_ref_edge = match side {
                    Top => *tile.orientation.borrow() == North,
                    Right => *tile.orientation.borrow() == East,
                    Bottom => *tile.orientation.borrow() == South,
                    Left => *tile.orientation.borrow() == West,
                };
                [(edge[0], is_ref_edge), (edge[1], is_ref_edge)]
            }
        }
    }

    /// Converts an edge color to a character sequence for printing the color in the
    /// terminal.
    fn to_terminal_chars(colors: &[(Color, bool); 2]) -> [String; 2] {
        use TerminalStyle::*;

        let mut result = [String::from(""), String::from("")];

        for (i, (color, is_main)) in colors.iter().enumerate() {
            // TODO: have color codes in constants!
            let to_print = if *is_main {
                match color {
                    Undefined => format!("{INV} {RESET}"),
                    Red => format!("{INV}{RED}R{RESET}"),
                    Blue => format!("{INV}{BLUE}B{RESET}"),
                    Green => format!("{INV}{GREEN}G{RESET}"),
                    Yellow => format!("{INV}{YELLOW}Y{RESET}"),
                }
            } else {
                match color {
                    Undefined => String::from(" "),
                    Red => format!("{RED}R{RESET}"),
                    Blue => format!("{BLUE}B{RESET}"),
                    Green => format!("{GREEN}G{RESET}"),
                    Yellow => format!("{YELLOW}Y{RESET}"),
                }
            };

            result[i] = to_print;
        }

        result
    }

    /// Prints a nicely rendered current state of the board, as well as the placement
    /// of the tiles in a concise form (tile ID and its orientation).
    fn pretty_print(&self) {
        use RelPlacement::*;
        use TerminalStyle::{BOLD, RESET};

        println!("┌─────┬─────┬─────╮");

        for row in 0..=2 {
            let tile0 = &self.slots[3 * row];
            let tile1 = &self.slots[3 * row + 1];
            let tile2 = &self.slots[3 * row + 2];

            // first character row
            print!("│");

            let colors_info = Self::colors_info(tile0, &Top);
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            let colors_info = Self::colors_info(tile1, &Top);
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            let colors_info = Self::colors_info(tile2, &Top);
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            println!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            // second character row
            print!("│");

            let colors_info = [
                Self::colors_info(tile0, &Left)[1],
                Self::colors_info(tile0, &Right)[0],
            ];
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let colors_info = [
                Self::colors_info(tile1, &Left)[1],
                Self::colors_info(tile1, &Right)[0],
            ];
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let colors_info = [
                Self::colors_info(tile2, &Left)[1],
                Self::colors_info(tile2, &Right)[0],
            ];
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            println!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            // third character row
            print!("│");

            let colors_info = [
                Self::colors_info(tile0, &Left)[0],
                Self::colors_info(tile0, &Right)[1],
            ];
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let colors_info = [
                Self::colors_info(tile1, &Left)[0],
                Self::colors_info(tile1, &Right)[1],
            ];
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let colors_info = [
                Self::colors_info(tile2, &Left)[0],
                Self::colors_info(tile2, &Right)[1],
            ];
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            println!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            // fourth character row
            print!("│");

            let colors_info = Self::colors_info(tile0, &Bottom);
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            let colors_info = Self::colors_info(tile1, &Bottom);
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            print!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            let colors_info = Self::colors_info(tile2, &Bottom);
            let strings_to_print = Self::to_terminal_chars(&colors_info);
            println!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            if row < 2 {
                println!("├─────┼─────┼─────┤");
            } else {
                println!("╰─────┴─────┴─────╯");
            }
        }

        println!("TILE PLACEMENT:");

        for (i, slot) in self.slots.iter().enumerate() {
            let tile_repr = match slot {
                None => "     ".to_string(),
                Some(tile) => {
                    let side_name = format!("{:?}", *tile.side_shown.borrow());
                    let orientation_name = format!("{:?}", *tile.orientation.borrow());
                    format!("{}({}{})", tile.id, &side_name[..1], &orientation_name[..1])
                }
            };
            print!(" {BOLD}{}{RESET}", tile_repr);

            if i % 3 == 2 {
                println!();
            }
        }

        println!();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
