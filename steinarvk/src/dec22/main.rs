use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::io::Read;

type Result<T> = std::result::Result<T, anyhow::Error>;

type FaceMap<T> = Map<Option<Map<T>>>;

trait FaceWrapper {
    fn step_forward_and_wrap(&self, state: &State, faces: &FaceMap<TileType>) -> Result<State>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TileType {
    Floor,
    Wall,
}

#[derive(Debug)]
struct State {
    face: (i32, i32),
    subpos: (i32, i32),
    facing: Direction,
}

#[derive(Debug)]
struct StateWithFaces<'a, T>
where
    T: FaceWrapper,
{
    state: State,
    faces: &'a FaceMap<TileType>,
    wrapper: T,
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    StepForward(i32),
    TurnRight(i32),
}

#[derive(Debug)]
struct Map<T> {
    number_of_rows: i32,
    number_of_columns: i32,
    row_data: Vec<Vec<T>>,
}

impl Direction {
    fn all() -> Vec<Direction> {
        vec![
            Direction::Right,
            Direction::Down,
            Direction::Left,
            Direction::Up,
        ]
    }

    fn from_i32(n: i32) -> Direction {
        let n = n.rem_euclid(4);
        match n {
            0 => Direction::Right,
            1 => Direction::Down,
            2 => Direction::Left,
            3 => Direction::Up,
            _ => panic!("invalid result of mod 4"),
        }
    }

    fn as_char(&self) -> char {
        match self {
            Direction::Right => '>',
            Direction::Down => 'v',
            Direction::Left => '<',
            Direction::Up => '^',
        }
    }

    fn to_i32(self) -> i32 {
        match self {
            Direction::Right => 0,
            Direction::Down => 1,
            Direction::Left => 2,
            Direction::Up => 3,
        }
    }

    fn opposite(&self) -> Direction {
        self.turn_right(2)
    }

    fn turn_right(&self, n: i32) -> Direction {
        Direction::from_i32(self.to_i32() + n)
    }

    fn dx(&self) -> i32 {
        match self {
            Direction::Right => 1,
            Direction::Left => -1,
            _ => 0,
        }
    }

    fn dy(&self) -> i32 {
        match self {
            Direction::Up => -1,
            Direction::Down => 1,
            _ => 0,
        }
    }

    fn step(&self, origin: (i32, i32)) -> (i32, i32) {
        (origin.0 + self.dx(), origin.1 + self.dy())
    }
}

impl Clone for State {
    fn clone(&self) -> Self {
        State {
            face: self.face,
            subpos: self.subpos,
            facing: self.facing,
        }
    }
}

impl Map<char> {
    fn from_string(s: &str) -> Result<Map<char>> {
        let rows: Vec<&str> = s.split('\n').collect();
        let number_of_rows = rows.len();
        let number_of_cols = rows[0].len();

        if !(rows.iter().all(|s| s.len() == number_of_cols)) {
            anyhow::bail!("rows are not all the same length");
        }

        let row_data = rows.iter().map(|row| row.chars().collect()).collect();

        Ok(Map {
            number_of_rows: number_of_rows.try_into().unwrap(),
            number_of_columns: number_of_cols.try_into().unwrap(),
            row_data,
        })
    }
}

impl<T> Map<T>
where
    T: Clone,
{
    fn new(number_of_columns: i32, number_of_rows: i32, starting_value: &T) -> Map<T> {
        let row_data = (0..number_of_rows)
            .map(|_| {
                (0..number_of_columns)
                    .map(|_| starting_value.clone())
                    .collect()
            })
            .collect();
        Map {
            number_of_rows,
            number_of_columns,
            row_data,
        }
    }

    fn at(&self, (col, row): (i32, i32)) -> Option<&T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&self.row_data[row as usize][col as usize])
    }

    fn at_mut(&mut self, (col, row): (i32, i32)) -> Option<&mut T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&mut self.row_data[row as usize][col as usize])
    }

    fn submap(&self, top_left: (i32, i32), size: (i32, i32)) -> Map<T> {
        let (x0, y0) = top_left;
        let x1 = x0 + size.0 - 1;
        let y1 = y0 + size.1 - 1;

        assert!(x0 >= 0 && x0 < self.number_of_columns);
        assert!(y0 >= 0 && y0 < self.number_of_rows);
        assert!(x1 >= 0 && x1 < self.number_of_columns);
        assert!(y1 >= 0 && y1 < self.number_of_rows);

        let mut rv: Vec<Vec<T>> = Vec::new();
        for y in y0..=y1 {
            let mut row: Vec<T> = Vec::new();
            for x in x0..=x1 {
                row.push(self.at((x, y)).unwrap().clone());
            }
            rv.push(row);
        }

        Map {
            number_of_rows: size.1,
            number_of_columns: size.0,
            row_data: rv,
        }
    }

    fn indexed_map<F, B>(&self, f: F) -> Map<B>
    where
        F: Fn((i32, i32), &T) -> B,
        B: Clone,
    {
        let row_data = self
            .row_data
            .iter()
            .enumerate()
            .map(|(row_index, row)| {
                row.iter()
                    .enumerate()
                    .map(|(col_index, value)| {
                        f(
                            (col_index.try_into().unwrap(), row_index.try_into().unwrap()),
                            value,
                        )
                    })
                    .collect()
            })
            .collect();
        Map {
            number_of_rows: self.number_of_rows,
            number_of_columns: self.number_of_columns,
            row_data,
        }
    }

    fn find(&self, needle: T) -> Vec<(i32, i32)>
    where
        T: PartialEq,
    {
        let mut rv: Vec<(i32, i32)> = Vec::new();

        for (y, row) in self.row_data.iter().enumerate() {
            for (x, value) in row.iter().enumerate() {
                if *value == needle {
                    rv.push((x.try_into().unwrap(), y.try_into().unwrap()));
                }
            }
        }

        rv
    }

    fn show<F>(&self, format_cell: F) -> String
    where
        F: Fn(&T) -> String,
    {
        let mut rv: Vec<String> = Vec::new();

        for row in &self.row_data {
            for col in row {
                rv.push(format_cell(col));
            }
            rv.push("\n".to_string());
        }

        rv.join("")
    }

    fn map<F, B>(&self, f: F) -> Map<B>
    where
        F: Fn(&T) -> B,
        B: Clone,
    {
        self.indexed_map(|_, x| f(x))
    }

    fn values(&self) -> Vec<T> {
        let mut rv = Vec::new();

        for row in &self.row_data {
            for col in row {
                rv.push(col.clone());
            }
        }

        rv
    }
}

impl<T> Clone for Map<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        self.map(|x| x.clone())
    }
}

fn pad(s: &str, n: usize) -> String {
    let mut rv = s.to_string();
    while rv.len() < n {
        rv += " ";
    }
    rv
}

struct Scenario {
    face_map: FaceMap<TileType>,
    instructions: Vec<Instruction>,
    start_state: State,
    full_map: Map<char>,
}

fn parse_scenario(s: &str) -> Result<Scenario> {
    let parts: Vec<&str> = s.split("\n\n").collect();

    if parts.len() != 2 {
        anyhow::bail!("invalid input: wrong number of components: {}", parts.len());
    }

    let max_line_len = parts[0].split('\n').map(|x| x.len()).max().unwrap();
    let padded_map: String = itertools::intersperse(
        parts[0].split('\n').map(|x| pad(x, max_line_len)),
        "\n".to_string(),
    )
    .collect();

    let full_map = Map::from_string(&padded_map)?;
    let height = full_map.number_of_rows;
    let width = full_map.number_of_columns;

    let (faces_wide, faces_tall) = if width > height { (4, 3) } else { (3, 4) };

    if width % faces_wide != 0 {
        anyhow::bail!(
            "width {} doesn't make sense for assumed {} faces wide",
            width,
            faces_wide
        );
    }
    let face_size = width / faces_wide;
    assert!(width == face_size * faces_wide);
    assert!(height == face_size * faces_tall);

    let mut facemap: FaceMap<TileType> = Map::new(faces_wide, faces_tall, &None);

    let mut starting_face: Option<(i32, i32)> = None;

    for j in 0..faces_tall {
        for i in 0..faces_wide {
            let submap = full_map.submap((i * face_size, j * face_size), (face_size, face_size));
            let empty = submap.values().iter().all(|x| *x == ' ');
            if !empty {
                if starting_face.is_none() {
                    starting_face = Some((i, j));
                }
                *facemap.at_mut((i, j)).unwrap() = Some(submap.map(|x| {
                    if *x == '#' {
                        TileType::Wall
                    } else {
                        TileType::Floor
                    }
                }));
            }
        }
    }

    let start_state = State {
        face: starting_face.unwrap(),
        subpos: (0, 0),
        facing: Direction::Right,
    };

    let path = parts[1];
    let mut steps = Vec::new();

    for token_pair in path.trim().split_inclusive(|x| x == 'L' || x == 'R') {
        let last_char = token_pair.chars().last().unwrap();
        let turning = match last_char {
            'L' => Some(Instruction::TurnRight(-1)),
            'R' => Some(Instruction::TurnRight(1)),
            ch => {
                assert!(ch.is_ascii_digit());
                None
            }
        };
        let numeric_token = if turning.is_some() {
            &token_pair[0..token_pair.len() - 1]
        } else {
            token_pair
        };
        let steps_forward: i32 = numeric_token.parse()?;
        steps.push(Instruction::StepForward(steps_forward));
        if let Some(ins) = turning {
            steps.push(ins);
        }
    }

    Ok(Scenario {
        face_map: facemap,
        instructions: steps,
        start_state,
        full_map,
    })
}

impl State {
    fn associate_faces<'a, T>(
        &self,
        faces: &'a FaceMap<TileType>,
        wrapper: T,
    ) -> StateWithFaces<'a, T>
    where
        T: FaceWrapper,
    {
        StateWithFaces {
            state: self.clone(),
            faces,
            wrapper,
        }
    }

    fn turn_right(&mut self, n: i32) {
        self.facing = self.facing.turn_right(n);
    }
}

impl<'a, T> StateWithFaces<'a, T>
where
    T: FaceWrapper,
{
    fn current_face_map(&self) -> &'a Map<TileType> {
        let (x, y) = self.state.face;
        self.faces.at((x, y)).unwrap().as_ref().unwrap()
    }

    fn step_forward(&mut self) -> Result<()> {
        let new_pos = self.state.facing.step(self.state.subpos);
        let face = self.current_face_map();

        match face.at(new_pos) {
            Some(TileType::Wall) => {
                // Nothing happens.
            }
            Some(TileType::Floor) => {
                self.state.subpos = new_pos;
            }
            None => {
                // Stepping onto a different face.. unless blocked.
                let new_state = self
                    .wrapper
                    .step_forward_and_wrap(&self.state, self.faces)?;
                self.state = new_state;
            }
        }

        Ok(())
    }

    fn apply(&mut self, ins: Instruction, viz_map: &mut Map<char>) -> Result<()> {
        assert!(*self.current_face_map().at(self.state.subpos).unwrap() == TileType::Floor);
        match ins {
            Instruction::StepForward(n) => {
                for _ in 0..n {
                    *viz_map.at_mut((self.full_x(), self.full_y())).unwrap() =
                        self.state.facing.as_char();
                    self.step_forward()?;
                    *viz_map.at_mut((self.full_x(), self.full_y())).unwrap() = 'X';
                }
            }
            Instruction::TurnRight(n) => {
                self.state.turn_right(n);
                *viz_map.at_mut((self.full_x(), self.full_y())).unwrap() =
                    self.state.facing.as_char();
            }
        }

        Ok(())
    }

    fn face_size(&self) -> i32 {
        let face_size = self.current_face_map().number_of_columns;
        assert!(face_size == self.current_face_map().number_of_rows);
        face_size
    }

    fn full_x(&self) -> i32 {
        self.state.face.0 * self.face_size() + self.state.subpos.0
    }

    fn full_y(&self) -> i32 {
        self.state.face.1 * self.face_size() + self.state.subpos.1
    }

    fn one_based_column(&self) -> i32 {
        self.full_x() + 1
    }

    fn one_based_row(&self) -> i32 {
        self.full_y() + 1
    }

    fn value(&self) -> i32 {
        let facing_value = self.state.facing.to_i32();
        self.one_based_row() * 1000 + 4 * self.one_based_column() + facing_value
    }
}

struct FlatWrapper {}

impl FlatWrapper {
    fn get_next_existing_face(
        &self,
        faces: &FaceMap<TileType>,
        face: (i32, i32),
        direction: Direction,
    ) -> (i32, i32) {
        let (mut i, mut j) = face;
        loop {
            i = (i + direction.dx() + faces.number_of_columns) % faces.number_of_columns;
            j = (j + direction.dy() + faces.number_of_rows) % faces.number_of_rows;
            if let Some(Some(_)) = faces.at((i, j)) {
                return (i, j);
            }
        }
    }
}

impl FaceWrapper for FlatWrapper {
    fn step_forward_and_wrap(&self, state: &State, faces: &FaceMap<TileType>) -> Result<State> {
        let next_face_coords = self.get_next_existing_face(faces, state.face, state.facing);
        let next_face = faces.at(next_face_coords).unwrap().as_ref().unwrap();
        let face_size = next_face.number_of_rows;
        let (mut x, mut y) = state.subpos;

        match state.facing {
            Direction::Left => {
                x = face_size - 1;
            }
            Direction::Right => {
                x = 0;
            }
            Direction::Up => {
                y = face_size - 1;
            }
            Direction::Down => {
                y = 0;
            }
        }

        match next_face.at((x, y)).unwrap() {
            TileType::Wall => Ok(state.clone()), // Blocked!
            TileType::Floor => Ok(State {
                face: next_face_coords,
                subpos: (x, y),
                facing: state.facing,
            }),
        }
    }
}

type FaceIndex = (i32, i32);
type EdgeIndex = (FaceIndex, Direction);

struct CubeWrapper {
    face_size: i32,
    edges_identified: HashMap<EdgeIndex, EdgeIndex>,
}

type IdentifiedEdgesSet = HashSet<(EdgeIndex, EdgeIndex)>;

struct OpenEdge {
    edge: EdgeIndex,
    from: QuatPair,
    to: QuatPair,
}

fn populate_map_from(
    visited: &mut Map<bool>,
    m: &mut HashMap<([i8; 8], [i8; 8]), EdgeIndex>,
    facemap: &Map<bool>,
    face: FaceIndex,
    q: QuatPair,
) -> Result<(IdentifiedEdgesSet, Vec<OpenEdge>)> {
    let key = q.normalize_as_top_face().hash_key();

    *visited.at_mut(face).unwrap() = true;

    let mut rv = Vec::new();
    let mut set = HashSet::new();

    for d in Direction::all() {
        let q = q.clone();
        let next = d.step(face);
        let next_qp = match d {
            Direction::Left => q.left(),
            Direction::Right => q.right(),
            Direction::Up => q.up(),
            Direction::Down => q.down(),
        };
        let next_key = next_qp.normalize_as_top_face().hash_key();
        if let Some(true) = facemap.at(next) {
            let edge = (face, d);
            let other_edge = (next, d.opposite());
            set.insert((edge, other_edge));
            set.insert((other_edge, edge));
            if !*visited.at(next).unwrap() {
                let (connections, open_edges) =
                    populate_map_from(visited, m, facemap, next, next_qp)?;
                set.extend(connections);
                rv.extend(open_edges);
            }
        } else {
            let who_i_am = key;
            let what_i_want = next_key;
            m.insert((who_i_am, what_i_want), (face, d));
            rv.push(OpenEdge {
                edge: (face, d),
                from: q,
                to: next_qp,
            });
        }
    }

    Ok((set, rv))
}

fn wrap_cube(unwrapped_map: &Map<bool>) -> Result<HashSet<(EdgeIndex, EdgeIndex)>> {
    let startpos = *unwrapped_map.find(true).first().unwrap();

    let mut visited: Map<bool> = Map::new(
        unwrapped_map.number_of_columns,
        unwrapped_map.number_of_rows,
        &false,
    );

    let mut m: HashMap<([i8; 8], [i8; 8]), EdgeIndex> = HashMap::new();
    let (mut connections, open_edges) = populate_map_from(
        &mut visited,
        &mut m,
        unwrapped_map,
        startpos,
        QuatPair::initial(),
    )?;

    for OpenEdge { edge, from, to } in open_edges {
        let who_i_am = from.normalize_as_top_face().hash_key();
        let what_i_want = to.normalize_as_top_face().hash_key();
        let counterpart_key = (what_i_want, who_i_am);
        let other_edge = match m.get(&counterpart_key) {
            Some(other_edge) => other_edge,
            None => {
                anyhow::bail!("invalid cube: unable to find connection from {:?}", edge);
            }
        };
        connections.insert((edge, *other_edge));
        connections.insert((*other_edge, edge));
    }

    if connections.len() != 24 {
        anyhow::bail!("failed to fold cube: somehow ended up with weird number of connections ({}), expected 24", connections.len());
    }

    Ok(connections)
}

impl CubeWrapper {
    fn identify_edges(&mut self, a: EdgeIndex, b: EdgeIndex) {
        self.edges_identified.insert(a, b);
        self.edges_identified.insert(b, a);
    }

    fn from(faces: &FaceMap<TileType>) -> Result<CubeWrapper> {
        let face_size: i32 = faces
            .values()
            .iter()
            .find_map(|x| x.as_ref().map(|face| face.number_of_columns))
            .unwrap();

        let mut rv = CubeWrapper {
            face_size,
            edges_identified: HashMap::new(),
        };

        let present_map = faces.map(|x| x.is_some());
        let edges = wrap_cube(&present_map)?;

        for (a, b) in edges {
            rv.identify_edges(a, b);
        }

        Ok(rv)
    }
}

impl CubeWrapper {
    fn position_on_edge(&self, edge: &EdgeIndex, pos: (i32, i32)) -> Result<i32> {
        let face_size = self.face_size;
        let (x, y) = pos;

        Ok(match edge.1 {
            Direction::Up => {
                if y != 0 {
                    anyhow::bail!("{:?} does not contain {:?}", edge, pos);
                }
                x
            }
            Direction::Right => {
                if x != (face_size - 1) {
                    anyhow::bail!("{:?} does not contain {:?}", edge, pos);
                }
                y
            }
            Direction::Down => {
                if y != (face_size - 1) {
                    anyhow::bail!("{:?} does not contain {:?}", edge, pos);
                }
                face_size - 1 - x
            }
            Direction::Left => {
                if x != 0 {
                    anyhow::bail!("{:?} does not contain {:?}", edge, pos);
                }
                face_size - 1 - y
            }
        })
    }

    fn coords_of_position_on_edge(&self, edge: &EdgeIndex, t: i32) -> Result<(i32, i32)> {
        let face_size = self.face_size;
        assert!(t >= 0 && t < face_size);

        Ok(match edge.1 {
            Direction::Up => (t, 0),
            Direction::Right => (face_size - 1, t),
            Direction::Down => (face_size - 1 - t, face_size - 1),
            Direction::Left => (0, face_size - 1 - t),
        })
    }
}

impl FaceWrapper for CubeWrapper {
    fn step_forward_and_wrap(&self, state: &State, faces: &FaceMap<TileType>) -> Result<State> {
        let edge = (state.face, state.facing);
        let other_edge = match self.edges_identified.get(&edge) {
            None => anyhow::bail!("edge {:?} is not identified with anything", edge),
            Some(x) => *x,
        };

        let t = self.position_on_edge(&edge, state.subpos)?;
        assert!(t >= 0 && t < self.face_size);

        let t = self.face_size - 1 - t;
        assert!(t >= 0 && t < self.face_size);

        let new_facing = other_edge.1.opposite();
        let new_pos = self.coords_of_position_on_edge(&other_edge, t)?;

        let next_face_coords = other_edge.0;
        let next_face = faces.at(next_face_coords).unwrap().as_ref().unwrap();

        match next_face.at(new_pos).unwrap() {
            TileType::Wall => Ok(state.clone()), // Blocked!
            TileType::Floor => Ok(State {
                face: next_face_coords,
                subpos: new_pos,
                facing: new_facing,
            }),
        }
    }
}

#[derive(Debug, Clone)]
struct Quat {
    values: [i8; 4],
}

impl Quat {
    fn new(values: [i8; 4]) -> Quat {
        Quat { values }
    }

    fn multiply(left: [i8; 4], right: [i8; 4]) -> [i8; 4] {
        let [a1, b1, c1, d1] = left;
        let [a2, b2, c2, d2] = right;

        [
            a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
            a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
            a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
            a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2,
        ]
    }

    fn up(&self) -> Quat {
        let q: [i8; 4] = [1, 1, 0, 0];
        let qi: [i8; 4] = [1, -1, 0, 0];
        let values = Self::multiply(Self::multiply(q, self.values), qi);
        Quat { values }.norm()
    }

    fn down(&self) -> Quat {
        let q: [i8; 4] = [-1, 1, 0, 0];
        let qi: [i8; 4] = [-1, -1, 0, 0];
        let values = Self::multiply(Self::multiply(q, self.values), qi);
        Quat { values }.norm()
    }

    fn right(&self) -> Quat {
        let q: [i8; 4] = [-1, 0, 1, 0];
        let qi: [i8; 4] = [-1, 0, -1, 0];
        let values = Self::multiply(Self::multiply(q, self.values), qi);
        Quat { values }.norm()
    }

    fn left(&self) -> Quat {
        let q: [i8; 4] = [1, 0, 1, 0];
        let qi: [i8; 4] = [1, 0, -1, 0];
        let values = Self::multiply(Self::multiply(q, self.values), qi);
        Quat { values }.norm()
    }

    fn turn(&self) -> Quat {
        let q: [i8; 4] = [1, 0, 0, 1];
        let qi: [i8; 4] = [1, 0, 0, -1];
        let values = Self::multiply(Self::multiply(q, self.values), qi);
        Quat { values }.norm()
    }

    fn norm(&self) -> Quat {
        let denom = self.values.iter().skip(1).map(|x| x.abs()).max().unwrap();
        if denom > 1 {
            assert!(denom == 2);
            let values = [
                self.values[0] / denom,
                self.values[1] / denom,
                self.values[2] / denom,
                self.values[3] / denom,
            ];
            Quat { values }
        } else {
            self.clone()
        }
    }
}

#[derive(Debug, Clone)]
struct QuatPair {
    a: Quat,
    b: Quat,
}

impl QuatPair {
    fn initial() -> QuatPair {
        let a = Quat::new([0, 1, 0, 0]);
        let b = Quat::new([0, 0, 1, 0]);
        QuatPair { a, b }
    }

    fn up(&self) -> QuatPair {
        let a = self.a.up();
        let b = self.b.up();
        QuatPair { a, b }
    }

    fn down(&self) -> QuatPair {
        let a = self.a.down();
        let b = self.b.down();
        QuatPair { a, b }
    }

    fn left(&self) -> QuatPair {
        let a = self.a.left();
        let b = self.b.left();
        QuatPair { a, b }
    }

    fn right(&self) -> QuatPair {
        let a = self.a.right();
        let b = self.b.right();
        QuatPair { a, b }
    }

    fn turn(&self) -> QuatPair {
        let a = self.a.turn();
        let b = self.b.turn();
        QuatPair { a, b }
    }

    fn values(&self) -> ([i8; 4], [i8; 4]) {
        (self.a.values, self.b.values)
    }

    fn normalize_as_top_face(&self) -> QuatPair {
        let a = self.turn();
        let b = a.turn();
        let c = b.turn();
        let values = [a.values(), b.values(), c.values()]
            .iter()
            .fold(self.values(), |a, b| a.max(*b));
        QuatPair {
            a: Quat { values: values.0 },
            b: Quat { values: values.1 },
        }
    }

    fn hash_key(&self) -> [i8; 8] {
        [
            self.a.values[0],
            self.a.values[1],
            self.a.values[2],
            self.a.values[3],
            self.b.values[0],
            self.b.values[1],
            self.b.values[2],
            self.b.values[3],
        ]
    }
}

fn test_cube_folding() {
    let mut num_valid = 0;
    let mut num_invalid = 0;
    for x in 0_u32..=0xffff_u32 {
        let m = Map::new(4, 4, &false).indexed_map(|(i, j), _| {
            assert!((0..4).contains(&i));
            assert!((0..4).contains(&j));
            let n: u32 = (i + j * 4).try_into().unwrap();
            let bit: u32 = 1 << n;
            (x & bit) != 0
        });
        if m.values().iter().filter(|x| **x).count() != 6 {
            continue;
        }
        if (0..4).all(|x| !(*m.at((x, 0)).unwrap())) {
            continue;
        }
        if (0..4).all(|y| !(*m.at((0, y)).unwrap())) {
            continue;
        }

        let connections = wrap_cube(&m);
        let valid = connections.is_ok();

        if valid {
            num_valid += 1;
        } else {
            num_invalid += 1;
        }

        let verdict = if valid { "valid" } else { "invalid" };

        if env::var("VALID_CUBES_ONLY").is_ok() && !valid {
            continue;
        }

        println!(
            "{}\n{}",
            m.show(|x| if *x { '#'.to_string() } else { '.'.to_string() }),
            verdict,
        );
    }

    println!(
        "Tried folding {} cubes; {} were valid and {} were invalid.",
        num_valid + num_invalid,
        num_valid,
        num_invalid
    );
}

fn main() -> Result<()> {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    let Scenario {
        face_map: faces,
        instructions: steps,
        start_state: start,
        full_map,
    } = parse_scenario(&buffer)?;

    let mut viz_map = full_map.clone();
    let mut current = start.associate_faces(&faces, FlatWrapper {});
    for ins in &steps {
        current.apply(*ins, &mut viz_map)?;
    }
    if env::var("SHOW_MAPS").is_ok() {
        println!("{}", viz_map.show(|x| x.to_string()));
    }
    println!("Answer part A: {}", current.value());

    let mut viz_map = full_map;
    let cube_wrapper = CubeWrapper::from(&faces)?;
    let mut current = start.associate_faces(&faces, cube_wrapper);
    for ins in &steps {
        current.apply(*ins, &mut viz_map)?;
    }

    if env::var("SHOW_MAPS").is_ok() {
        println!("{}", viz_map.show(|x| x.to_string()));
    }
    println!("Answer part B: {}", current.value());

    if env::var("EXPLORE_CUBE_FOLDING").is_ok() {
        test_cube_folding();
    }

    Ok(())
}
