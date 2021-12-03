create table digits_test (
    "digit" varchar(5)
);
truncate table digits_test;
copy digits_test(digit) from '/home/andehen/oda/dev/adventofcode/andehen/day03/test.txt' CSV;

select digit from digits_test;

drop view parsed_test cascade;
create view parsed_test as (
    select
        concat(
            round(avg((substr(digit, 1, 1))::decimal)) 
            ,round(avg((substr(digit, 2, 1))::decimal))
            ,round(avg((substr(digit, 3, 1))::decimal))
            ,round(avg((substr(digit, 4, 1))::decimal))
            ,round(avg((substr(digit, 5, 1))::decimal))
        ) as most_common
    from digits_test
);

create or replace view flipped_digits_test as (
    select
        replace(replace(replace(digit, '1', 'b'), '0', '1'), 'b', '0') as digit
    from digits_test
);

drop view parsed2_test;
create view parsed2_test as (
    select
        most_common,
        replace(replace(replace(most_common, '1', 'b'), '0', '1'), 'b', '0') as least_common
    from parsed_test
);

select most_common, least_common from parsed2_test;
select most_common::bit(5)::int, least_common::bit(5)::int from parsed2_test;
select most_common::bit(5)::int * least_common::bit(5)::int from parsed2_test;

select * from digits_test;

with recursive o2_rating as (

    select
        0 as n,
        digit,
        count(*) over (),
        round(avg((substr(digit, 1, 1))::decimal) over ())::int::text as next_mc
    from digits_test
    union
    select
        n+1,
        digit,
        count(*) over() as count,
        round(avg((substr(digit, n+2, 1))::decimal) over ())::int::text
    from o2_rating
    where substr(digit, n+1, 1) = next_mc and count > 2
)

-- select *, digit::bit(5)::int from o2_rating;

select digit::bit(5)::int from o2_rating where count <= 2 order by count desc, substr(digit, n+1, 1) desc limit 1;

with recursive co2_rating as (

    select
        0 as n,
        digit,
        count(*) over (),
        round(avg((substr(digit, 1, 1))::decimal) over ())::int::text as next_mc
    from digits_test
    union
    select
        n+1,
        digit,
        count(*) over() as count,
        round(avg((substr(digit, n+2, 1))::decimal) over ())::int::text
    from co2_rating
    where substr(digit, n+1, 1) != next_mc and count > 2
)

select 
        *, replace(replace(replace(digit, '1', 'b'), '0', '1'), 'b', '0')::bit(5)::int as co2_rating, digit::bit(5)::int
from co2_rating where count <= 2 order by count desc, substr(digit, n+1, 1) limit 1;
