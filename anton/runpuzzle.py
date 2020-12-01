import argparse


def init_argparse() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        usage="%(prog)s [DAY] [PART]",
        description="Run the specified advent_of_code daily puzzle",
    )
    parser.add_argument("day")
    parser.add_argument("part", nargs="?", default="main")
    return parser


def main() -> None:
    parser = init_argparse()
    args = parser.parse_args()
    if not args.day:
        print("Please specify which day to run")

    daily_module = __import__(f"{args.day}")
    print(getattr(daily_module, args.part)())


if __name__ == "__main__":
    main()
