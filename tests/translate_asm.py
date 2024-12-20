import re


def main() -> None:
    with open("tmp.s") as f:
        s = f.read()

    s = re.sub(r"r(..)", r"%e\1", s)
    s = re.sub(r"%eet", r"ret", s)
    s = re.sub(r"(\d+)", r"$\1", s)
    s = re.sub(r"\[(.+)\]", r"(\1)", s)
    s = re.sub(r"([^ \n]+), ([^ \n]+)", r"\2, \1", s)
    s = re.sub(r"8", r"4", s)

    # instructions = ["mov", "sub", "push" "pop"]
    # for i in instructions:
    #     s = re.sub(i, i + "q", s)

    print(s)


if __name__ == "__main__":
    main()
