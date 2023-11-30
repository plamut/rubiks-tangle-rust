use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

static mut COMBOS_TRIED: u32 = 0;

pub fn solve() -> u32 {
    let unplaced_tiles = define_tiles();
    let mut board = Board::new();

    let n_solutions = find_solutions(&mut board, unplaced_tiles);

    println!("*** COMBINATIONS CHECKED: \x1b[1m{}\x1b[0m", unsafe {
        COMBOS_TRIED
    });

    n_solutions
}

fn find_solutions(board: &mut Board, unplaced_tiles: HashSet<Rc<Tile>>) -> u32 {
    // println!("Called find_solutions, {} unplaced tiles left", unplaced_tiles.len());
    // board.pretty_print();

    // pojdi skozi vse tiles ki se niso polozeni
    // - za vsak tile:
    //     - za vse mozne rotacije (front/back, orientacija)
    //         - polozi tile na board, ce lahko
    //             - ce je zadnji tile -> imamo resitev, print board, povecaj n_solutions
    //             - ce ni zadnji tile, resu podproblem find_solutions(0 z novim boardom in enim
    //               manj unplaced_tiles)
    //         - ce ne mores postavit, poskusi naslednjo rotacijo

    // let mut my_copy: HashSet<&Tile> = HashSet::new();
    // my_copy.extend(unplaced_tiles.iter());

    // let mut my_copy: Vec<_> = unplaced_tiles.drain().collect();
    // let mut unplaced_tiles_cp = unplaced_tiles.clone();

    // for tile in my_copy.iter() {
    // for ref mut tile in &mut my_copy {
    // for ref mut tile in &mut my_copy {
    // for (i, tile_ref) in unplaced_tiles.iter().enumerate() {

    let mut n_solutions = 0;

    let mut tiles_to_check = unplaced_tiles.clone();

    for tile in unplaced_tiles.iter().cloned() {
        // todo: for every possible roation (fornt/back, 4 orientations)
        for side in [TileSide::Front, TileSide::Back].iter() {
            // println!("Changing side shown to {:?}...", side);
            // board.pretty_print();
            tile.set_side_shown(&side);
            // println!("...changed side shown to {:?}", side);
            // board.pretty_print();
            // println!();
            // let _x = 1;

            for orientation in [North, East, South, West].iter() {
                // println!("Changing orientation to {:?}...", orientation);
                // board.pretty_print();

                tile.set_orientation(&orientation);

                // println!("...changed orientation to {:?}", orientation);
                // board.pretty_print();

                unsafe { COMBOS_TRIED += 1 };
                if !board.can_place(&*tile) {
                    continue;
                }

                // println!("Placing a board tile...");
                board.place_tile(Rc::clone(&tile));
                tiles_to_check.remove(&tile);

                // board.pretty_print();

                // TODO: check if solutions.. count not None!
                if tiles_to_check.len() == 0 {
                    n_solutions += 1;
                    println!("Found a solution!");
                    board.pretty_print();
                } else {
                    n_solutions += find_solutions(board, tiles_to_check.clone());
                    // println!("...back from recursive call");
                }

                // board.pretty_print();
                // println!("Removing a board tile...");
                tiles_to_check.insert(Rc::clone(&tile));
                board.remove_tile();
                // board.pretty_print();
            }
        }
    }

    n_solutions
}

struct Board {
    // These are the indexes of the board's slots:
    //   0 1 2
    //   3 4 5
    //   6 7 8
    slots: Vec<Option<Rc<Tile>>>,
    pub first_empty_idx: usize,
}

type NeighborSlot = HashMap<Orientation, Option<usize>>;

impl Board {
    fn new() -> Board {
        Board {
            slots: [None, None, None, None, None, None, None, None, None].to_vec(),
            first_empty_idx: 0,
        }
    }

    // TODO: tile representation should change...
    // e.g. tile.visible_side.edges[North].first_color
    //
    // TODO: BUG! indexing does not take orientation into account
    // Better to add a method on the card can_place(other_tile, Side),
    // where Side is (top, left, bottom, right)
    // And then the tile considers its own orientation and the other's orientation and
    // returns a bool
    //
    // TODO: use tile's can_adjoin(other)
    fn can_place(&self, tile: &Tile) -> bool {
        // indexes of all neighbor slots for each slot
        let adjacencies = [
            // TODO: module level const?
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

        fn idx_adjust(orientation: &Orientation) -> usize {
            match orientation {
                North => 0,
                East => 3,
                South => 2,
                West => 1,
            }
        }

        let slot_idx = self.first_empty_idx;
        let adjacent_slots = adjacencies[slot_idx];

        // Northern neighbor
        let adjacent_slot_idx = adjacent_slots[0];
        match adjacent_slot_idx {
            None => {}
            Some(slot_idx) => {
                let adjacent_tile = &self.slots[slot_idx];
                match adjacent_tile {
                    None => {}
                    Some(existing_tile) => {
                        let visible_colors =
                            existing_tile.colors[&*existing_tile.side_shown.borrow()];

                        // TODO: pick edge based on orientation!
                        let adj = idx_adjust(&*existing_tile.orientation.borrow());
                        let existing_edge = visible_colors[(2 + adj) % 4];

                        let adj = idx_adjust(&*tile.orientation.borrow());
                        let new_edge = tile.colors[&*tile.side_shown.borrow()][(0 + adj) % 4]; // North edge

                        if new_edge[0] != existing_edge[1] || new_edge[1] != existing_edge[0] {
                            return false;
                        }
                    }
                }
            }
        }

        // Eastern neighbor
        let adjacent_slot_idx = adjacent_slots[1];
        match adjacent_slot_idx {
            None => {}
            Some(slot_idx) => {
                let adjacent_tile = &self.slots[slot_idx];
                match adjacent_tile {
                    None => {}
                    Some(existing_tile) => {
                        let visible_colors =
                            existing_tile.colors[&*existing_tile.side_shown.borrow()];

                        let adj = idx_adjust(&*existing_tile.orientation.borrow());
                        let existing_edge = visible_colors[(3 + adj) % 4]; // West edge

                        let adj = idx_adjust(&*tile.orientation.borrow());
                        let new_edge = tile.colors[&*tile.side_shown.borrow()][(1 + adj) % 4]; // East edge

                        if new_edge[0] != existing_edge[1] || new_edge[1] != existing_edge[0] {
                            return false;
                        }
                    }
                }
            }
        }

        // Southern neighbor
        let adjacent_slot_idx = adjacent_slots[2];
        match adjacent_slot_idx {
            None => {}
            Some(slot_idx) => {
                let adjacent_tile = &self.slots[slot_idx];
                match adjacent_tile {
                    None => {}
                    Some(existing_tile) => {
                        let visible_colors =
                            existing_tile.colors[&*existing_tile.side_shown.borrow()];

                        let adj = idx_adjust(&*existing_tile.orientation.borrow());
                        let existing_edge = visible_colors[(0 + adj) % 4]; // North edge

                        let adj = idx_adjust(&*tile.orientation.borrow());
                        let new_edge = tile.colors[&*tile.side_shown.borrow()][(2 + adj) % 4]; // South edge

                        if new_edge[0] != existing_edge[1] || new_edge[1] != existing_edge[0] {
                            return false;
                        }
                    }
                }
            }
        }

        // Western neighbor
        let adjacent_slot_idx = adjacent_slots[3];
        match adjacent_slot_idx {
            None => {}
            Some(slot_idx) => {
                let adjacent_tile = &self.slots[slot_idx];
                match adjacent_tile {
                    None => {}
                    Some(existing_tile) => {
                        let visible_colors =
                            existing_tile.colors[&*existing_tile.side_shown.borrow()];

                        let adj = idx_adjust(&*existing_tile.orientation.borrow());
                        let existing_edge = visible_colors[(1 + adj) % 4]; // East edge

                        let adj = idx_adjust(&*tile.orientation.borrow());
                        let new_edge = tile.colors[&*tile.side_shown.borrow()][(3 + adj) % 4]; // West edge

                        if new_edge[0] != existing_edge[1] || new_edge[1] != existing_edge[0] {
                            return false;
                        }
                    }
                }
            }
        }

        true
    }

    fn place_tile(&mut self, tile: Rc<Tile>) {
        /////////////
        let _is_ok = self.can_place(&*tile);
        ///
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

    fn pretty_print(&self) {
        // println!();

        // println!("See the board below, first empty index is {}", self.first_empty_idx);
        println!("┌─────┬─────┬─────╮");

        for row in 0..=2 {
            let tile0 = &self.slots[3 * row];
            let tile1 = &self.slots[3 * row + 1];
            let tile2 = &self.slots[3 * row + 2];

            // TODO: better: get colors for all edges?
            // Or just switch to index-based representation?

            // first character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let edge_colors = Self::edge_colors_top(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            let edge_colors = Self::edge_colors_top(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            let edge_colors = Self::edge_colors_top(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            println!(" {} {} │", strings_to_print[0], strings_to_print[1]);

            // second character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let edge_colors = Self::edge_colors_lr_upper(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let edge_colors = Self::edge_colors_lr_upper(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let edge_colors = Self::edge_colors_lr_upper(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            println!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            // third character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let edge_colors = Self::edge_colors_lr_lower(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let edge_colors = Self::edge_colors_lr_lower(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            let edge_colors = Self::edge_colors_lr_lower(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            println!("{}   {}│", strings_to_print[0], strings_to_print[1]);

            // fourth character row
            print!("│");

            // TODO: map chars to terminal colors... use iterator probably?
            let edge_colors = Self::edge_colors_bottom(tile0);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            let edge_colors = Self::edge_colors_bottom(tile1);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
            print!(" {} {} │", strings_to_print[1], strings_to_print[0]);

            let edge_colors = Self::edge_colors_bottom(tile2);
            let strings_to_print = Self::to_terminal_chars(&edge_colors);
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
                    format!("{}({}{})", tile.id, &side_name[..1], &orientation_name[..1],)
                }
            };
            print!(" \x1b[1m{}\x1b[0m", tile_repr);

            if i % 3 == 2 {
                println!();
            }
        }

        println!();
    }

    fn edge_colors_top(tile: &Option<Rc<Tile>>) -> [(Color, bool); 2] {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&*tile.side_shown.borrow()];

                let top_edge_idx = match *tile.orientation.borrow() {
                    North => 0,
                    East => 3,
                    South => 2,
                    West => 1,
                };

                return [
                    (
                        shown_edges[top_edge_idx][0],
                        *tile.orientation.borrow() == North,
                    ),
                    (
                        shown_edges[top_edge_idx][1],
                        *tile.orientation.borrow() == North,
                    ),
                ];
            }
            None => {
                return [(Color::Undefined, false), (Color::Undefined, false)];
            }
        }
    }

    fn edge_colors_bottom(tile: &Option<Rc<Tile>>) -> [(Color, bool); 2] {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&*tile.side_shown.borrow()];

                let bottom_edge_idx = match *tile.orientation.borrow() {
                    North => 2,
                    East => 1,
                    South => 0,
                    West => 3,
                };

                return [
                    (
                        shown_edges[bottom_edge_idx][0],
                        *tile.orientation.borrow() == South,
                    ),
                    (
                        shown_edges[bottom_edge_idx][1],
                        *tile.orientation.borrow() == South,
                    ),
                ];
            }
            None => {
                return [(Color::Undefined, false), (Color::Undefined, false)];
            }
        }
    }

    // TODO: need two bools! for each color, they are on separate edges
    // TODO: change to return colors for all at once? and idx based representation?
    fn edge_colors_lr_upper(tile: &Option<Rc<Tile>>) -> [(Color, bool); 2] {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&*tile.side_shown.borrow()];

                let left_edge_idx = match *tile.orientation.borrow() {
                    North => 3,
                    East => 2,
                    South => 1,
                    West => 0,
                };
                let right_edge_idx = match *tile.orientation.borrow() {
                    North => 1,
                    East => 0,
                    South => 3,
                    West => 2,
                };

                return [
                    (
                        shown_edges[left_edge_idx][1],
                        *tile.orientation.borrow() == West,
                    ),
                    (
                        shown_edges[right_edge_idx][0],
                        *tile.orientation.borrow() == East,
                    ),
                ];
            }
            None => {
                return [(Color::Undefined, false), (Color::Undefined, false)];
            }
        }
    }

    // TODO: need two bools! for each color, they are on separate edges
    // TODO: change to return colors for all at once? and idx based representation?
    fn edge_colors_lr_lower(tile: &Option<Rc<Tile>>) -> [(Color, bool); 2] {
        match tile {
            Some(tile) => {
                let shown_edges = tile.colors[&*tile.side_shown.borrow()];

                let left_edge_idx = match *tile.orientation.borrow() {
                    North => 3,
                    East => 2,
                    South => 1,
                    West => 0,
                };
                let right_edge_idx = match *tile.orientation.borrow() {
                    North => 1,
                    East => 0,
                    South => 3,
                    West => 2,
                };

                return [
                    (
                        shown_edges[left_edge_idx][0],
                        *tile.orientation.borrow() == West,
                    ),
                    (
                        shown_edges[right_edge_idx][1],
                        *tile.orientation.borrow() == East,
                    ),
                ];
            }
            None => {
                return [(Color::Undefined, false), (Color::Undefined, false)];
            }
        }
    }

    fn to_terminal_chars(colors: &[(Color, bool); 2]) -> [String; 2] {
        let mut result = [String::from(""), String::from("")];

        for (i, (color, is_main)) in colors.iter().enumerate() {
            let to_print: String;

            // TODO: have color codes in constants!
            if *is_main {
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
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
    Above,
    Right,
    Below,
    Left,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
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
// 7   2
// 6   3
//  5 4
//
// NOTE: When the indexes are used to enumerate the tile's back side, we pretend that we
// can "see through" the tile when placed face up (as oposed to an alternative where we
// would physically turn the tile face down and then enumerating the colors that we see)
type TileFace = [[Color; 2]; 4]; // 4 edgese, each with two colors

#[derive(Clone, Debug)]
struct Tile {
    pub id: u32,
    pub side_shown: RefCell<TileSide>,
    pub orientation: RefCell<Orientation>, // orientation of the reference edge
    pub colors: HashMap<TileSide, TileFace>, // TODO: let it be a tuple... with exactly two items
}

use std::hash::{Hash, Hasher};

impl Hash for Tile {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for edge in self.colors[&Front] {
            edge[0].hash(state);
            edge[1].hash(state);
        }
        for edge in self.colors[&Back] {
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

    fn can_adjoin(&self, other_tile: &Tile, side: &RelPlacement) -> bool {
        let our_edges = &self.colors[&*self.side_shown.borrow()];
        let others_edges = &other_tile.colors[&*other_tile.side_shown.borrow()];

        match side {
            RelPlacement::Above => {
                let our_edge = match *self.orientation.borrow() {
                    // our top edge
                    North => our_edges[0],
                    East => our_edges[3],
                    South => our_edges[2],
                    West => our_edges[1],
                };
                let other_edge = match *other_tile.orientation.borrow() {
                    // other's bottom edge
                    North => others_edges[2],
                    East => others_edges[1],
                    South => others_edges[0],
                    West => others_edges[3],
                };

                if our_edge[0] != other_edge[1] || our_edge[1] != other_edge[0] {
                    return false;
                }
            }
            RelPlacement::Right => {
                let our_edge = match *self.orientation.borrow() {
                    // our right edge
                    North => our_edges[1],
                    East => our_edges[0],
                    South => our_edges[3],
                    West => our_edges[2],
                };
                let other_edge = match *other_tile.orientation.borrow() {
                    // other's left edge
                    North => others_edges[3],
                    East => others_edges[2],
                    South => others_edges[1],
                    West => others_edges[0],
                };

                if our_edge[0] != other_edge[1] || our_edge[1] != other_edge[0] {
                    return false;
                }
            }
            RelPlacement::Below => {
                let our_edge = match *self.orientation.borrow() {
                    // our bottom edge
                    North => our_edges[2],
                    East => our_edges[1],
                    South => our_edges[0],
                    West => our_edges[3],
                };
                let other_edge = match *other_tile.orientation.borrow() {
                    // oher's top edge
                    North => others_edges[0],
                    East => others_edges[3],
                    South => others_edges[2],
                    West => others_edges[1],
                };

                if our_edge[0] != other_edge[1] || our_edge[1] != other_edge[0] {
                    return false;
                }
            }
            RelPlacement::Left => {
                let our_edge = match *self.orientation.borrow() {
                    // our left edge
                    North => our_edges[3],
                    East => our_edges[2],
                    South => our_edges[1],
                    West => our_edges[0],
                };
                let other_edge = match *other_tile.orientation.borrow() {
                    // oher's right edge
                    North => others_edges[1],
                    East => others_edges[0],
                    South => others_edges[3],
                    West => others_edges[2],
                };

                if our_edge[0] != other_edge[1] || our_edge[1] != other_edge[0] {
                    return false;
                }
            }
        }

        true
    }
}

fn define_tiles() -> HashSet<Rc<Tile>> {
    let mut tiles = HashSet::new();

    let mut sequence = (1..).into_iter();

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

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
