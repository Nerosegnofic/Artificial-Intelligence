import copy
from functools import lru_cache

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
        [(1, -1), (-1, 1)]
    ]

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
    return 0 <= row < size and 0 <= col < size and board[row][col] == '.'

# to be enhanced
def evaluate(board, player):
    opponent = 'O' if player == 'X' else 'X'

    def evaluate_line(x, y, dx, dy):
        line = []
        for i in range(-4, 5):
            r, c = x + dx * i, y + dy * i
            if 0 <= r < len(board) and 0 <= c < len(board):
                line.append(board[r][c])
            else:
                line.append(None)
        return score_line(line, player, opponent)

    def score_line(line, player, opponent):
        score = 0
        window = 5
        for i in range(len(line) - window + 1):
            segment = line[i:i+window]
            if None in segment:
                continue
            if segment.count(player) == 5:
                score += 100000
            elif segment.count(player) == 4 and segment.count('.') == 1:
                score += 10000
            elif segment.count(player) == 3 and segment.count('.') == 2:
                score += 500
            elif segment.count(player) == 2 and segment.count('.') == 3:
                score += 100
            elif segment.count(player) == 1 and segment.count('.') == 4:
                score += 10

            if segment.count(opponent) == 5:
                score -= 100000
            elif segment.count(opponent) == 4 and segment.count('.') == 1:
                score -= 10000
            elif segment.count(opponent) == 3 and segment.count('.') == 2:
                score -= 500
            elif segment.count(opponent) == 2 and segment.count('.') == 3:
                score -= 100
            elif segment.count(opponent) == 1 and segment.count('.') == 4:
                score -= 10
        return score

    total_score = 0
    directions = [(1, 0), (0, 1), (1, 1), (1, -1)]
    for i in range(len(board)):
        for j in range(len(board)):
            if board[i][j] != '.':
                continue
            for dx, dy in directions:
                total_score += evaluate_line(i, j, dx, dy)

    return total_score

memo = {}
BASE = 9973
MOD = 10 ** 9 + 7

def flatten_board_rolling_hash(board):
    h = 0
    for row in board:
        for cell in row:
            h = (h * BASE + ord(cell)) % MOD
    return h

def minimax(board, depth, maximizingPlayer, player, opponent):
    board_key = flatten_board_rolling_hash(board)
    if board_key in memo:
        return memo[board_key]

    score = evaluate(board, player)
    if depth == 0 or abs(score) == 100 or not get_valid_moves(board):
        memo[board_key] = score
        return score

    if maximizingPlayer:
        maxEval = float('-inf')
        for move in get_valid_moves(board):
            if not has_neighbor(board, move, opponent):
                continue
            board[move[0]][move[1]] = player
            eval = minimax(board, depth - 1, False, player, opponent)
            board[move[0]][move[1]] = '.'
            maxEval = max(maxEval, eval)
        memo[board_key] = maxEval
        return maxEval
    else:
        minEval = float('inf')
        for move in get_valid_moves(board):
            if not has_neighbor(board, move, player):
                continue
            board[move[0]][move[1]] = opponent
            eval = minimax(board, depth - 1, True, player, opponent)
            board[move[0]][move[1]] = '.'
            minEval = min(minEval, eval)
        memo[board_key] = minEval
        return minEval

def get_valid_moves(board):
    return [(i, j) for i in range(len(board)) for j in range(len(board)) if board[i][j] == '.']

def has_neighbor(board, move, lst):
    dx = [0, 0, -1, 1, 1, 1, -1, 1]
    dy = [-1, 1, 0, 0, 1, -1, 1, -1]
    for i in range(8):
        r = move[0] + dx[i]
        c = move[1] + dy[i]
        if 0 <= r < len(board) and 0 <= c < len(board) and board[r][c] == lst:
            return True
    return False

def get_ai_move(board, ai_player, user_player):
    best_score = float('-inf')
    best_move = None
    for move in get_valid_moves(board):
        if not has_neighbor(board, move, user_player) and (move[0] or move[1]):
            continue

        board[move[0]][move[1]] = ai_player
        score = minimax(board, 2, False, ai_player, user_player)
        board[move[0]][move[1]] = '.'
        print(move, score, end='\n')
        if score > best_score:
            best_score = score
            best_move = move
    return best_move

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

def play_with_AI():
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

    while True:
        user_player = input("Choose your symbol (X or O): ").upper()
        if user_player not in ['X', 'O']:
            print("Invalid input. Please choose 'X' or 'O'.")
            continue
        break

    ai_player = 'O' if user_player == 'X' else 'X'
    current_player = user_player if input("Enter 1 to play first, 0 to play second: ") == '1' else ai_player
    move_count = 0
    max_moves = size * size

    print("\nWelcome to Gomoku vs AI!")
    print_board(board)

    while move_count < max_moves:
        print(f"\nPlayer {current_player}'s turn:")
        if current_player == user_player:
            try:
                move = input("Enter your move (row col): ")
                row, col = map(int, move.split())
                if not is_valid_move(board, row, col):
                    print("Invalid move. Try again.")
                    continue
                board[row][col] = user_player
            except Exception:
                print("Invalid input. Try again.")
                continue
        else:
            move = get_ai_move(board, ai_player, user_player)
            if move:
                board[move[0]][move[1]] = ai_player

        print_board(board)
        move_count += 1

        last_move = move if current_player == ai_player else (row, col)
        if check_win(board, last_move[0], last_move[1], current_player):
            print(f"\nPlayer {current_player} wins!")
            return

        current_player = ai_player if current_player == user_player else user_player

    print("\nGame over! It's a draw.")

    print("Thanks for playing!")

def main():
    print("\n===== Welcome to Gomoku Game =====")
    print("1. Human vs Human")
    print("2. Human vs AI")
    
    while True:
        try:
            choice = int(input("\nSelect game mode (1-2): "))
            if choice == 1:
                play_gomoku()
                break
            elif choice == 2:
                play_with_AI()
                break
            else:
                print("Invalid choice. Please enter 1 or 2.")
        except ValueError:
            print("Invalid input. Please enter a number.")

if __name__ == "__main__":
    main()
