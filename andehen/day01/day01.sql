-- run with sqlite3 < day01.sql
create table depths("depth" int);
.mode csv
.import input.txt depths

with prev_depths as (
    select
        depth,
        lag(depth, 1, null) over () as prev_depth
    from depths
)

select
    count(*)
from prev_depths
where depth > prev_depth;

with sliding_window as (
    select
        sum(depth) over (rows between 2 preceding and current row) as sum_depth,
        count(depth) over (rows between 2 preceding and current row) as depth_count
    from depths
),
prev_sums as (
    select
        sum_depth,
        lag(sum_depth, 1, null) over () as prev_sum_depth,
        lag(depth_count, 1, null) over () as prev_depth_count
    from sliding_window
)
select
    count(*)
from prev_sums
where sum_depth > prev_sum_depth and prev_depth_count > 2;
