# Agent Guidelines for Advent of Code Solutions

## Key Focus Areas

### 1. Solve Advent of Code Problems Using Rust
- All solutions should be implemented in Rust
- Follow Rust best practices and idioms
- Leverage Rust's type system and safety features

### 2. Code Should Be Simple, Performant, and Fast to Write
- Prioritize clarity and readability
- Write straightforward, maintainable code
- Optimize for performance where it matters, but don't over-engineer
- Focus on getting working solutions quickly

### 3. Use Utilities
- Leverage the utilities module (`src/utils.rs`) for common operations:
  - `read_to_lines()` - Read input file to vector of strings
  - `read_to_int_vectors()` - Parse input to vector of integer vectors
  - `read_to_matrix()` - Read input as 2D character matrix
  - `check_string_match_in_matrix()` - Safe matrix element checking
  - `rotate_90()` - Rotate direction vectors 90 degrees
- Avoid duplicating utility functionality
- Extend utilities if needed for common patterns

### 4. Run with `cargo run`
- Ensure all solutions can be executed with `cargo run`
- Solutions should be accessible through the main `solve()` function in `lib.rs`
- Add new day modules to `lib.rs` and implement the solution function

## Project Structure
- `src/main.rs` - Entry point that calls `solve()`
- `src/lib.rs` - Module declarations and `solve()` function routing
- `src/utils.rs` - Shared utility functions
- `src/day_X.rs` - Individual day solutions (replace X with day number)

