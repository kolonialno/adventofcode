from jinja2 import Environment


def render(sql: str) -> str:
    env = Environment()
    render = env.from_string(sql).render()
    return render
