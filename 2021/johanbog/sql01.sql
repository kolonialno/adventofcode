create table input(distance INTEGER);
.separator ","
.import input01.txt input

with lagged_distances as (
    select
        distance,
        lead(distance) over () as next_distance
    from input
)

select count(*) from lagged_distances
where next_distance>distance
;

with input_with_row_numbers as (
    select
        row_number() over win1 as rn,
        distance
    from input
    window win1 as ()
),

grouped_by_three as (
    select sum(distance) over win1 as summed_distance
    from input_with_row_numbers
    WINDOW win1 as (order by rn range between current row and 2 following)
),

diff_to_next as (
    select
        summed_distance,
        lead(summed_distance) over () as next_summed_distance
    from grouped_by_three
)

select count(*)
from diff_to_next
where next_summed_distance > summed_distance
;
