-- run with: psql -f day03.sql
create temp table digits (
    "digit" varchar(12)
);
copy digits(digit) from '/home/andehen/oda/dev/adventofcode/andehen/day03/input.txt' CSV;

create temp view most_common as (
    select
        concat(
            round(avg((substr(digit, 1, 1))::decimal))
            ,round(avg((substr(digit, 2, 1))::decimal))
            ,round(avg((substr(digit, 3, 1))::decimal))
            ,round(avg((substr(digit, 4, 1))::decimal))
            ,round(avg((substr(digit, 5, 1))::decimal))
            ,round(avg((substr(digit, 6, 1))::decimal))
            ,round(avg((substr(digit, 7, 1))::decimal))
            ,round(avg((substr(digit, 8, 1))::decimal))
            ,round(avg((substr(digit, 9, 1))::decimal))
            ,round(avg((substr(digit, 10, 1))::decimal))
            ,round(avg((substr(digit, 11, 1))::decimal))
            ,round(avg((substr(digit, 12, 1))::decimal))
        ) as most_common
    from digits
);

create temp view common as (
    select
        most_common,
        replace(replace(replace(most_common, '1', 'b'), '0', '1'), 'b', '0') as least_common
    from most_common
);

select most_common::bit(12)::int * least_common::bit(12)::int from common as part_1;

with recursive o2_rating as (
    select
        0 as n,
        digit,
        count(*) over () as count,
        round(avg((substr(digit, 1, 1))::decimal) over ())::int::text as next_mc
    from digits
    union
    select
        n+1,
        digit,
        count(*) over (),
        round(avg((substr(digit, n+2, 1))::decimal) over ())::int::text
    from o2_rating 
    where substr(digit, n+1, 1) = next_mc and count > 2
),

co2_rating as (
    select
        0 as n,
        digit,
        count(*) over () as count,
        round(avg((substr(digit, 1, 1))::decimal) over ())::int::text as next_mc
    from digits
    union
    select
        n+1,
        digit,
        count(*) over (),
        round(avg((substr(digit, n+2, 1))::decimal) over ())::int::text
    from co2_rating 
    where substr(digit, n+1, 1) != next_mc and count > 2
)

select
    (select digit::bit(12)::int from o2_rating where count <= 2 order by count desc, substr(digit, n+1, 1) desc limit 1)
    *
    (select digit::bit(12)::int from co2_rating where count <= 2 order by count desc, substr(digit, n+1, 1) limit 1)
as part_2;
