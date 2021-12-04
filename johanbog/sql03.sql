with input as (select * from read_csv_auto(
    'input.txt',
    delim=',',
    header=False,
    columns={'numbers':'VARCHAR'})
),

padded as (
    -- Have to pad the strings because leading zeroes are dropped
    select lpad(numbers, 12, 0) as numbers
from input
),

a as (
    {% for i in range(12) %}
    select
        mode(coalesce(split(numbers::VARCHAR,'')[{{i}}]::INTEGER,0)) as mode_num
    from padded
    {%- if not loop.last %} union all {% endif -%}
    {% endfor %}
),

b as (
    select
        row_number() over () -1 as rn,
        mode_num
    from a
    order by rn desc
)

select
    sum(mode_num * (2 ** rn))
    * sum((1-mode_num) * (2 ** rn))
from b

