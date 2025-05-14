import random

BASE = 9973
MOD = 10 ** 9 + 7
memo = {}


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


def is_valid_move(board, row, col):
    size = len(board)
    return 0 <= row < size and 0 <= col < size and board[row][col] == '.'


def check_win(board, row, col, player):
    directions = [(0, 1), (1, 0), (1, 1), (1, -1)]
    size = len(board)

    for dx, dy in directions:
        count = 1

        for i in range(1, 5):
            r, c = row + dx * i, col + dy * i
            if 0 <= r < size and 0 <= c < size and board[r][c] == player:
                count += 1
            else:
                break

        for i in range(1, 5):
            r, c = row - dx * i, col - dy * i
            if 0 <= r < size and 0 <= c < size and board[r][c] == player:
                count += 1
            else:
                break

        if count >= 5:
            return True

    return False



def get_valid_moves(board):
    return [(i, j) for i in range(len(board)) for j in range(len(board)) if board[i][j] == '.']


def has_neighbor(board, move, target):
    dx = [0, 0, -1, 1, 1, 1, -1, -1]
    dy = [-1, 1, 0, 0, 1, -1, 1, -1]
    for i in range(8):
        r = move[0] + dx[i]
        c = move[1] + dy[i]
        if 0 <= r < len(board) and 0 <= c < len(board) and board[r][c] == target:
            return True
    return False


def flatten_board_rolling_hash(board):
    h = 0
    for row in board:
        for cell in row:
            h = (h * BASE + ord(cell)) % MOD
    return h


def evaluate(board, player):
    opponent = 'O' if player == 'X' else 'X'
    directions = [(1, 0), (0, 1), (1, 1), (1, -1)]
    size = len(board)
    score = 0

    def score_segment(segment):
        player_count = segment.count(player)
        opponent_count = segment.count(opponent)
        empty_count = segment.count('.')

        if player_count == 5:
            return 1_000_000
        if opponent_count == 5:
            return -1_000_000
        if player_count == 4 and empty_count == 1:
            return 100_000
        if opponent_count == 4 and empty_count == 1:
            return -100_000
        if player_count == 3 and empty_count == 2:
            return 10_000
        if opponent_count == 3 and empty_count == 2:
            return -10_000
        if player_count == 2 and empty_count == 3:
            return 1_000
        if opponent_count == 2 and empty_count == 3:
            return -1_000
        if player_count == 1 and empty_count == 4:
            return 100
        if opponent_count == 1 and empty_count == 4:
            return -100
        return 0

    for x in range(size):
        for y in range(size):
            for dx, dy in directions:
                segment = []
                for i in range(5):
                    nx, ny = x + dx * i, y + dy * i
                    if 0 <= nx < size and 0 <= ny < size:
                        segment.append(board[nx][ny])
                    else:
                        segment.append(None)
                if None not in segment:
                    score += score_segment(segment)

    return score


def has_four_aligned(board, row, col, player):
    directions = [(1, 0), (0, 1), (1, 1), (1, -1)]
    for dx, dy in directions:
        count = 1
        for sign in [-1, 1]:
            for i in range(1, 5):
                r = row + sign * dx * i
                c = col + sign * dy * i
                if 0 <= r < len(board) and 0 <= c < len(board) and board[r][c] == player:
                    count += 1
                else:
                    break
        if count >= 4:
            return True
    return False


def minimax(board, depth, maximizing, player, opponent):
    key = flatten_board_rolling_hash(board)
    if key in memo:
        return memo[key]

    score = evaluate(board, player)
    if depth == 0 or abs(score) >= 100000 or not get_valid_moves(board):
        memo[key] = score
        return score

    if maximizing:
        maxEval = float('-inf')
        for move in get_valid_moves(board):
            if not has_neighbor(board, move, opponent) and not has_four_aligned(board, move[0], move[1], player):
                continue
            board[move[0]][move[1]] = player
            eval = minimax(board, depth - 1, False, player, opponent)
            board[move[0]][move[1]] = '.'
            maxEval = max(maxEval, eval)
        memo[key] = maxEval if maxEval != float('-inf') else score
        return memo[key]
    else:
        minEval = float('inf')
        for move in get_valid_moves(board):
            if not has_neighbor(board, move, player) and not has_four_aligned(board, move[0], move[1], opponent):
                continue
            board[move[0]][move[1]] = opponent
            eval = minimax(board, depth - 1, True, player, opponent)
            board[move[0]][move[1]] = '.'
            minEval = min(minEval, eval)
        memo[key] = minEval if minEval != float('inf') else score
        return memo[key]


def alpha_beta(board, depth, alpha, beta, maximizing, player, opponent):
    key = flatten_board_rolling_hash(board)
    if key in memo:
        return memo[key]

    score = evaluate(board, player)
    if depth == 0 or abs(score) >= 100000 or not get_valid_moves(board):
        memo[key] = score
        return score

    if maximizing:
        maxEval = float('-inf')
        for move in get_valid_moves(board):
            if not has_neighbor(board, move, opponent) and not has_four_aligned(board, move[0], move[1], player):
                continue
            board[move[0]][move[1]] = player
            eval = alpha_beta(board, depth - 1, alpha, beta, False, player, opponent)
            board[move[0]][move[1]] = '.'
            maxEval = max(maxEval, eval)
            alpha = max(alpha, eval)
            if beta <= alpha:
                break
        memo[key] = maxEval if maxEval != float('-inf') else score
        return memo[key]
    else:
        minEval = float('inf')
        for move in get_valid_moves(board):
            if not has_neighbor(board, move, player) and not has_four_aligned(board, move[0], move[1], opponent):
                continue
            board[move[0]][move[1]] = opponent
            eval = alpha_beta(board, depth - 1, alpha, beta, True, player, opponent)
            board[move[0]][move[1]] = '.'
            minEval = min(minEval, eval)
            beta = min(beta, eval)
            if beta <= alpha:
                break
        memo[key] = minEval if minEval != float('inf') else score
        return memo[key]


def get_ai_move(board, ai_player, user_player, ai_type="minimax"):
    best_score = float('-inf')
    best_move = None

    if all(cell == '.' for row in board for cell in row):
        return random.randint(0, len(board) - 1), random.randint(0, len(board) - 1)

    for move in get_valid_moves(board):
        if not has_neighbor(board, move, user_player) and not has_four_aligned(board, move[0], move[1], ai_player) and (
                move[0] > 0 or move[1] > 0):
            continue
        board[move[0]][move[1]] = ai_player
        if ai_type == "minimax":
            score = minimax(board, 3, False, ai_player, user_player)
        else:
            score = alpha_beta(board, 3, float('-inf'), float('inf'), False, ai_player, user_player)
        board[move[0]][move[1]] = '.'
        if score > best_score:
            best_score = score
            best_move = move
    return best_move


def play_gomoku():
    size = get_board_size()
    board = [['.' for _ in range(size)] for _ in range(size)]
    player = 'X'
    move_count = 0

    print("\nHuman vs Human Gomoku")
    print_board(board)

    while move_count < size * size:
        print(f"\nPlayer {player}'s turn:")
        row, col = get_move_input(size, board)
        board[row][col] = player
        print_board(board)
        if check_win(board, row, col, player):
            print(f"\nPlayer {player} wins!")
            return
        player = 'O' if player == 'X' else 'X'
        move_count += 1

    print("\nGame Over: Draw.")


def play_with_AI():
    size = get_board_size()
    board = [['.' for _ in range(size)] for _ in range(size)]
    user_player = get_user_symbol()
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
                row, col = map(int, input("Enter your move (row col): ").split())
                if not is_valid_move(board, row, col):
                    print("Invalid move. Try again.")
                    continue
                board[row][col] = user_player
            except:
                print("Invalid input. Try again.")
                continue
        else:
            move = get_ai_move(board, ai_player, user_player)
            if move:
                board[move[0]][move[1]] = ai_player
        print_board(board)
        last_move = move if current_player == ai_player else (row, col)
        if check_win(board, last_move[0], last_move[1], current_player):
            print(f"\nPlayer {current_player} wins!")
            return
        current_player = ai_player if current_player == user_player else user_player
        move_count += 1
    print("\nGame over! It's a draw.")


def play_ai_vs_ai():
    size = get_board_size()
    board = [['.' for _ in range(size)] for _ in range(size)]
    ai_player1 = 'X'
    ai_player2 = 'O'

    print("\nAI vs AI Gomoku!")
    print("Choose AI 1 type (1 for Minimax, 2 for Alpha-Beta):")
    ai1_type = input("AI 1 type: ")

    print("Choose AI 2 type (1 for Minimax, 2 for Alpha-Beta):")
    ai2_type = input("AI 2 type: ")

    current_player = ai_player1
    move_count = 0
    max_moves = size * size
    print_board(board)

    while move_count < max_moves:
        print(f"\nPlayer {current_player}'s turn:")

        if current_player == ai_player1:
            ai_type = "minimax" if ai1_type == '1' else "alpha_beta"
        else:
            ai_type = "minimax" if ai2_type == '1' else "alpha_beta"

        move = get_ai_move(board, current_player, ai_player1 if current_player == ai_player2 else ai_player2, ai_type)

        if move:
            board[move[0]][move[1]] = current_player

        print_board(board)

        if check_win(board, move[0], move[1], current_player):
            print(f"\nPlayer {current_player} wins!")
            return

        current_player = ai_player1 if current_player == ai_player2 else ai_player2
        move_count += 1

    print("\nGame over! It's a draw.")


def get_board_size():
    while True:
        try:
            size = int(input("Enter board size (>=5): "))
            if size >= 5:
                return size
            print("Board must be at least 5x5.")
        except ValueError:
            print("Invalid input.")


def get_user_symbol():
    while True:
        symbol = input("Choose X or O: ").upper()
        if symbol in ['X', 'O']:
            return symbol
        print("Invalid symbol.")


def get_move_input(size, board):
    while True:
        try:
            move = input("Enter move (row col): ")
            row, col = map(int, move.split())
            if 0 <= row < size and 0 <= col < size and board[row][col] == '.':
                return row, col
            print("Invalid move. Try again.")
        except ValueError:
            print("Invalid input. Please enter row and column numbers.")


def main():
    print("Welcome to Gomoku")
    print("1. Human vs Human")
    print("2. Human vs AI")
    print("3. AI vs AI")

    while True:
        choice = input("Choose mode (1, 2, or 3): ")
        if choice == '1':
            play_gomoku()
            break
        elif choice == '2':
            play_with_AI()
            break
        elif choice == '3':
            play_ai_vs_ai()
            break
        else:
            print("Invalid choice.")

if __name__ == "__main__":
    main()
