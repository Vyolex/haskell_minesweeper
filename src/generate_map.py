import numpy as np

BOMB = -1


def generate(n: int, m: int, n_mines: int) -> np.array:
    # Beginner (8x8 with 10 mines), Intermediate (16x16 with 40 mines), and Expert (16x30 with 99 mines)
    assert n_mines < n*m
    minesweeper_map = np.zeros((n, m), dtype=int)

    for _ in range(n_mines):
        new_mine = False
        while not new_mine:
            mine_x = np.random.randint(n)
            mine_y = np.random.randint(m)

            new_mine = minesweeper_map[mine_x][mine_y] != BOMB

        minesweeper_map[mine_x][mine_y] = BOMB

        for d_x in [-1, 0, 1]:
            if mine_x + d_x < 0 or mine_x + d_x >= n:
                continue
            for d_y in [-1, 0, 1]:
                if mine_y + d_y < 0 or mine_y + d_y >= m:
                    continue
                if minesweeper_map[mine_x + d_x][mine_y + d_y] != BOMB:
                    minesweeper_map[mine_x + d_x][mine_y + d_y] += 1

    return minesweeper_map


if __name__ == "__main__":
    print(generate(16, 16, 40))
