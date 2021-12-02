-- run with: duckdb < day02.sql
create sequence id start 0 increment 1;
create table cmds (
    "id" int primary key,
    "cmd" string
);
insert into cmds(id, cmd) select nextval('id'), * from read_csv_auto('input.txt');

create view parsed_cmds as (
    select
        id,
        string_split(cmd, ' ')[0] as cmd,
        string_split(cmd, ' ')[1]::int as value
    from cmds
);

select sum(value) filter (where cmd = 'forward') * ( sum(value) filter (where cmd = 'down') - sum(value) filter (where cmd = 'up')) as part_1
from parsed_cmds;

with cmds_aim as (
    select
        id,
        value,
        cmd,
        sum(case when cmd = 'down' then value when cmd = 'up' then -value else 0 end) over (order by id) as aim
    from parsed_cmds
),
x_y_diffs as (
    select 
        case when cmd = 'forward' then value else 0 end as x_diff,
        case when cmd = 'forward' then aim * value else 0 end as y_diff
    from cmds_aim
)

select sum(x_diff) * sum(y_diff) as part_2 from x_y_diffs
