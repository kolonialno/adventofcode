create table input_table(dir VARCHAR, distance INT);
.separator " "
.import input02.txt input_table


select
    sum(case dir when 'up' then -distance when 'down' then distance else 0 end)
    *sum(case dir when 'forward' then distance else 0 end)

from input_table
;

with a as (select row_number() over () rn, * from input_table),
b as (
select
    *,
    sum(case dir when 'up' then -distance when 'down' then distance end) over win1 as aim,
    sum(case dir when 'forward' then distance else 0 end) over win1 as position

from a
WINDOW win1 as (order by rn range between unbounded preceding and current row)--1 preceding)
),
c as (

select
row_number() over () as rn,
dir,distance,'|aim',aim,'|pos',position,'|depth',
sum(case dir when 'forward' then distance * aim else 0 end) over win1 as depth,
position*(sum(case dir when 'forward' then distance * aim else 0 end) over win1) as answer
from b
WINDOW win1 as (order by rn range between unbounded preceding and current row)--1 preceding)
)
select answer from c order by rn desc limit 1
;
