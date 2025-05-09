def print_board(board):
    print("   ", end="")
    for i in range(len(board)):
        print(f"{i:2d} ", end="")
    print()

    for i in range(len(board)):
        print(f"{i:2d} |", end="")
        for j in range(len(board)):
            print(f" {board[i][j]} ", end="")
        print("|")


def check_win(board, row, col, player):
    directions = [
        [(0, 1), (0, -1)],
        [(1, 0), (-1, 0)],
        [(1, 1), (-1, -1)],
        [(1, -1), (-1, 1)]]

    size = len(board)

    for dir_pair in directions:
        count = 1

        for dx, dy in dir_pair:
            for i in range(1, 5):
                r, c = row + dx * i, col + dy * i
                if 0 <= r < size and 0 <= c < size and board[r][c] == player:
                    count += 1
                else:
                    break

        if count >= 5:
            return True

    return False


def is_valid_move(board, row, col):
    size = len(board)
    if not (0 <= row < size and 0 <= col < size):
        return False
    if board[row][col] != '.':
        return False
    return True


def play_gomoku():
    while True:
        try:
            size = int(input("Enter the board size (e.g., 15 for 15x15): "))
            if size < 5:
                print("Board size must be at least 5x5.")
                continue
            break
        except ValueError:
            print("Please enter a valid number.")

    board = [['.' for _ in range(size)] for _ in range(size)]

    current_player = 'X'
    move_count = 0
    max_moves = size * size

    print("\nWelcome to Gomoku!")
    print("Black (X) goes first, White (O) goes second.")
    print("Enter moves as 'row col' (e.g., '7 8').")
    print_board(board)

    while move_count < max_moves:
        print(f"\nPlayer {current_player}'s turn:")
        try:
            move = input("Enter your move (row col): ")
            row, col = map(int, move.split())

            if not is_valid_move(board, row, col):
                print("Invalid move. Try again.")
                continue

            board[row][col] = current_player
            move_count += 1

            print_board(board)

            if check_win(board, row, col, current_player):
                print(f"\nPlayer {current_player} wins!")
                break

            current_player = 'O' if current_player == 'X' else 'X'

        except (ValueError, IndexError):
            print("Invalid input. Please enter row and column as numbers separated by a space.")

    if move_count == max_moves:
        print("\nGame over! It's a draw.")

    print("Thanks for playing!")


if __name__ == "__main__":
    play_gomoku()
