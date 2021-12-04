with padded as (
    -- Have to pad the strings because leading zeroes are dropped
    select lpad(numbers, 5, 0) as numbers
from input
),

a0 as (
    with calc_mode as (
        select
            (avg(split(numbers::VARCHAR, '')[0]::INTEGER)>=0.5)::INTEGER as oxygen,
            (avg(split(numbers::VARCHAR, '')[0]::INTEGER)<0.5)::INTEGER as co2
        from padded
    )
    select
        (select *
            from (
                select numbers from padded
                where split(numbers::VARCHAR,'')[0]::INTEGER = (select oxygen from calc_mode)
            )
        ) as oxygen_numbers,
        (select *
            from (
                select numbers from padded
                where split(numbers::VARCHAR,'')[0]::INTEGER = (select co2 from calc_mode)
            )
        ) as co2_numbers
),

{% for i in range(1, 5) %}
a{{i}} as (
    with calc_mode as (
        select
            (avg(split(oxygen_numbers::VARCHAR, '')[{{i}}]::INTEGER)>=0.5)::INTEGER as oxygen,
            (avg(split(co2_numbers::VARCHAR, '')[{{i}}]::INTEGER)<0.5)::INTEGER as co2
        from a{{i-1}}
    )
    select
        (select *
            from (
                select oxygen_numbers from a{{i-1}}
                where split(oxygen_numbers::VARCHAR,'')[{{i}}]::INTEGER = (select oxygen from calc_mode)
            )
        ) as oxygen_numbers,
        (select *
            from (
                select co2_numbers from a{{i-1}}
                where split(co2_numbers::VARCHAR,'')[{{i}}]::INTEGER = (select co2 from calc_mode)
            )
        ) as co2_numbers
),
{% endfor %}

b as (
    select * from a4
)

select * from b
