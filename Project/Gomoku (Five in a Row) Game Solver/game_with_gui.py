import tkinter as tk
from tkinter import messagebox, ttk
import random
import copy

BASE = 9973
MOD = 10 ** 9 + 7
memo = {}


class GomokuGUI:
    def __init__(self, master):
        self.master = master
        self.master.title("Gomoku Game")
        self.master.resizable(False, False)

        self.size = 15
        self.cell_size = 40
        self.board = None
        self.current_player = None
        self.game_over = False
        self.user_player = None
        self.ai_player = None
        self.game_mode = None
        self.ai_type1 = None
        self.ai_type2 = None

        self.create_main_menu()

    def create_main_menu(self):
        for widget in self.master.winfo_children():
            widget.destroy()

        self.master.geometry("400x300")

        title_label = tk.Label(self.master, text="Welcome to Gomoku", font=("Arial", 18, "bold"))
        title_label.pack(pady=20)

        human_vs_human_btn = tk.Button(self.master, text="Human vs Human", width=20, height=2,
                                       command=lambda: self.setup_game("human_vs_human"))
        human_vs_human_btn.pack(pady=10)

        human_vs_ai_btn = tk.Button(self.master, text="Human vs AI", width=20, height=2,
                                    command=lambda: self.setup_game("human_vs_ai"))
        human_vs_ai_btn.pack(pady=10)

        ai_vs_ai_btn = tk.Button(self.master, text="AI vs AI", width=20, height=2,
                                 command=lambda: self.setup_game("ai_vs_ai"))
        ai_vs_ai_btn.pack(pady=10)

    def setup_game(self, mode):
        self.game_mode = mode

        for widget in self.master.winfo_children():
            widget.destroy()

        self.master.geometry("400x400")

        title_label = tk.Label(self.master, text=f"Game Setup - {mode.replace('_', ' ').title()}",
                               font=("Arial", 14, "bold"))
        title_label.pack(pady=10)

        size_frame = tk.Frame(self.master)
        size_frame.pack(pady=10)

        tk.Label(size_frame, text="Board Size: 15").grid(row=0, column=0, padx=5)

        size_var = tk.StringVar(value="15")

        if mode == "human_vs_ai":
            symbol_frame = tk.Frame(self.master)
            symbol_frame.pack(pady=10)

            tk.Label(symbol_frame, text="Your Symbol:").grid(row=0, column=0, padx=5)

            symbol_var = tk.StringVar(value="X")
            symbol_dropdown = ttk.Combobox(symbol_frame, textvariable=symbol_var, values=["X", "O"], width=5,
                                           state="readonly")
            symbol_dropdown.grid(row=0, column=1, padx=5)

            first_move_frame = tk.Frame(self.master)
            first_move_frame.pack(pady=10)

            first_move_var = tk.StringVar(value="You")
            tk.Radiobutton(first_move_frame, text="You play first", variable=first_move_var, value="You").grid(row=0,
                                                                                                               column=0,
                                                                                                               padx=5)
            tk.Radiobutton(first_move_frame, text="AI plays first", variable=first_move_var, value="AI").grid(row=0,
                                                                                                              column=1,
                                                                                                              padx=5)

            ai_algo_frame = tk.Frame(self.master)
            ai_algo_frame.pack(pady=10)

            tk.Label(ai_algo_frame, text="AI Algorithm:").grid(row=0, column=0, padx=5)

            ai_algo_var = tk.StringVar(value="alpha_beta")
            tk.Radiobutton(ai_algo_frame, text="Minimax", variable=ai_algo_var, value="minimax").grid(row=0, column=1,
                                                                                                      padx=5)
            tk.Radiobutton(ai_algo_frame, text="Alpha-Beta", variable=ai_algo_var, value="alpha_beta").grid(row=0,
                                                                                                            column=2,
                                                                                                            padx=5)

        elif mode == "ai_vs_ai":
            ai1_frame = tk.Frame(self.master)
            ai1_frame.pack(pady=10)

            tk.Label(ai1_frame, text="AI 1 (X) Algorithm:").grid(row=0, column=0, padx=5)

            ai1_algo_var = tk.StringVar(value="alpha_beta")
            tk.Radiobutton(ai1_frame, text="Minimax", variable=ai1_algo_var, value="minimax").grid(row=0, column=1,
                                                                                                   padx=5)
            tk.Radiobutton(ai1_frame, text="Alpha-Beta", variable=ai1_algo_var, value="alpha_beta").grid(row=0,
                                                                                                         column=2,
                                                                                                         padx=5)

            ai2_frame = tk.Frame(self.master)
            ai2_frame.pack(pady=10)

            tk.Label(ai2_frame, text="AI 2 (O) Algorithm:").grid(row=0, column=0, padx=5)

            ai2_algo_var = tk.StringVar(value="alpha_beta")
            tk.Radiobutton(ai2_frame, text="Minimax", variable=ai2_algo_var, value="minimax").grid(row=0, column=1,
                                                                                                   padx=5)
            tk.Radiobutton(ai2_frame, text="Alpha-Beta", variable=ai2_algo_var, value="alpha_beta").grid(row=0,
                                                                                                         column=2,
                                                                                                         padx=5)

        button_frame = tk.Frame(self.master)
        button_frame.pack(pady=20)

        back_btn = tk.Button(button_frame, text="Back", width=10, command=self.create_main_menu)
        back_btn.grid(row=0, column=0, padx=10)

        start_btn = tk.Button(button_frame, text="Start Game", width=10)

        if mode == "human_vs_human":
            start_btn.config(command=lambda: self.start_game(int(size_var.get())))
        elif mode == "human_vs_ai":
            start_btn.config(command=lambda: self.start_game(
                int(size_var.get()),
                symbol_var.get(),
                first_move_var.get() == "You",
                ai_algo_var.get()
            ))
        elif mode == "ai_vs_ai":
            start_btn.config(command=lambda: self.start_game(
                int(size_var.get()),
                ai_type1=ai1_algo_var.get(),
                ai_type2=ai2_algo_var.get()
            ))

        start_btn.grid(row=0, column=1, padx=10)

    def start_game(self, size, user_symbol=None, user_first=None, ai_algo=None, ai_type1=None, ai_type2=None):
        self.size = size
        self.board = [['.' for _ in range(size)] for _ in range(size)]
        self.game_over = False

        if self.game_mode == "human_vs_human":
            self.current_player = 'X'
        elif self.game_mode == "human_vs_ai":
            self.user_player = user_symbol
            self.ai_player = 'O' if user_symbol == 'X' else 'X'
            self.current_player = user_symbol if user_first else self.ai_player
            self.ai_type1 = ai_algo
        elif self.game_mode == "ai_vs_ai":
            self.current_player = 'X'
            self.ai_type1 = ai_type1
            self.ai_type2 = ai_type2

        global memo
        memo = {}

        window_size = self.size * self.cell_size + 200
        self.master.geometry(f"{window_size}x{window_size}")

        for widget in self.master.winfo_children():
            widget.destroy()

        self.game_frame = tk.Frame(self.master)
        self.game_frame.pack(pady=10)

        canvas_size = self.size * self.cell_size
        self.canvas = tk.Canvas(self.game_frame, width=canvas_size, height=canvas_size, bg="bisque")
        self.canvas.pack()

        self.draw_board()

        self.info_frame = tk.Frame(self.master)
        self.info_frame.pack(pady=10)

        self.player_label = tk.Label(self.info_frame, text=f"Current Player: {self.current_player}", font=("Arial", 12))
        self.player_label.grid(row=0, column=0, padx=10, pady=5)

        back_btn = tk.Button(self.info_frame, text="Back to Menu", command=self.create_main_menu)
        back_btn.grid(row=1, column=0, padx=10, pady=5)

        self.canvas.bind("<Button-1>", self.on_canvas_click)

        self.master.after(100, self.check_ai_turn)

    def draw_board(self):
        cs = self.cell_size
        for i in range(self.size):
            self.canvas.create_line(0, i * cs, self.size * cs, i * cs)
            self.canvas.create_line(i * cs, 0, i * cs, self.size * cs)

        for i in range(self.size):
            for j in range(self.size):
                if self.board[i][j] == 'X':
                    self.draw_x(i, j)
                elif self.board[i][j] == 'O':
                    self.draw_o(i, j)

    def draw_x(self, row, col):
        cs = self.cell_size
        x_center = col * cs + cs // 2
        y_center = row * cs + cs // 2
        radius = cs // 2 - 5
        self.canvas.create_oval(x_center - radius, y_center - radius,
                                x_center + radius, y_center + radius,
                                fill="black", outline="black")

    def draw_o(self, row, col):
        cs = self.cell_size
        x_center = col * cs + cs // 2
        y_center = row * cs + cs // 2
        radius = cs // 2 - 5
        self.canvas.create_oval(x_center - radius, y_center - radius,
                                x_center + radius, y_center + radius,
                                fill="white", outline="black", width=2)

    def on_canvas_click(self, event):
        if self.game_over:
            return

        if (self.game_mode == "ai_vs_ai" or
                (self.game_mode == "human_vs_ai" and self.current_player != self.user_player)):
            return

        col = event.x // self.cell_size
        row = event.y // self.cell_size

        if not self.is_valid_move(row, col):
            return

        self.make_move(row, col)

        self.master.after(100, self.check_ai_turn)

    def check_ai_turn(self):
        if self.game_over:
            return

        if self.game_mode == "ai_vs_ai" or (self.game_mode == "human_vs_ai" and self.current_player == self.ai_player):
            self.make_ai_move()

            if self.game_mode == "ai_vs_ai" and not self.game_over:
                self.master.after(500, self.check_ai_turn)

    def make_ai_move(self):
        if self.game_mode == "human_vs_ai":
            ai_type = self.ai_type1
            opponent = self.user_player
        else:
            if self.current_player == 'X':
                ai_type = self.ai_type1
            else:
                ai_type = self.ai_type2
            opponent = 'O' if self.current_player == 'X' else 'X'

        move = self.get_ai_move(self.board, self.current_player, opponent, ai_type)

        if move:
            self.make_move(move[0], move[1])

    def make_move(self, row, col):
        self.board[row][col] = self.current_player

        if self.current_player == 'X':
            self.draw_x(row, col)
        else:
            self.draw_o(row, col)

        if self.check_win(self.board, row, col, self.current_player):
            self.game_over = True
            if self.game_mode == "human_vs_ai":
                if self.current_player == self.user_player:
                    message = "Congratulations! You win!"
                else:
                    message = "AI wins! Better luck next time."
            else:
                message = f"Player {self.current_player} wins!"

            messagebox.showinfo("Game Over", message)
            return

        if self.is_board_full():
            self.game_over = True
            messagebox.showinfo("Game Over", "It's a draw!")
            return

        self.current_player = 'O' if self.current_player == 'X' else 'X'
        self.player_label.config(text=f"Current Player: {self.current_player}")

    def is_board_full(self):
        for row in self.board:
            if '.' in row:
                return False
        return True

    def is_valid_move(self, row, col):
        return 0 <= row < self.size and 0 <= col < self.size and self.board[row][col] == '.'

    ############## ABOVE IS GUI ############# DOWN IS THE GAME #############

    def check_win(self, board, row, col, player):
        directions = [(0, 1), (1, 0), (1, 1), (1, -1), (-1, 1), (0, -1), (-1, 0), (-1, -1)]
        for dx, dy in directions:
            count = 1
            for i in range(1, 6):
                x, y = row + dx * i, col + dy * i
                if 0 <= x < len(board) and 0 <= y < len(board[0]) and board[x][y] == player:
                    count += 1
                else:
                    break
            for i in range(1, 6):
                x, y = row - dx * i, col - dy * i
                if 0 <= x < len(board) and 0 <= y < len(board[0]) and board[x][y] == player:
                    count += 1
                else:
                    break
            if count >= 5:
                return True
        return False

    def get_valid_moves(self, board):
        return [(i, j) for i in range(len(board)) for j in range(len(board)) if board[i][j] == '.']

    def has_neighbor(self, board, move, target):
        dx = [0, 0, -1, 1, 1, 1, -1, -1]
        dy = [-1, 1, 0, 0, 1, -1, 1, -1]
        for i in range(8):
            r = move[0] + dx[i]
            c = move[1] + dy[i]
            if 0 <= r < len(board) and 0 <= c < len(board) and board[r][c] == target:
                return True
        return False

    def flatten_board_rolling_hash(self, board):
        h = 0
        for row in board:
            for cell in row:
                h = (h * BASE + ord(cell)) % MOD
        return h

    def evaluate(self, board, player):
        opponent = 'O' if player == 'X' else 'X'
        score = 0
        pattern_weights = {
            'win5': 10_000_000,
            'double_open4': 8_000_000,
            'open4': 5_000_000,
            'halfopen4': 3_000_000,
            'double_open3': 2_000_000,
            'open3': 1_000_000,
            'halfopen3': 500_000,
            'open2': 200_000
        }

        def check_line(x, y, dx, dy, player):
            consecutive = 0
            opens = 0
            streaks = []
            for i in range(-5, 6):
                nx, ny = x + dx*i, y + dy*i
                if 0 <= nx < len(board) and 0 <= ny < len(board[0]):
                    if board[nx][ny] == player:
                        consecutive += 1
                        if consecutive >= 5:
                            return pattern_weights['win5']
                    else:
                        if consecutive > 0:
                            left_open = 0 <= (x + dx*(i-consecutive-1)) < len(board) and 0 <= (y + dy*(i-consecutive-1)) < len(board[0]) and board[x + dx*(i-consecutive-1)][y + dy*(i-consecutive-1)] == '.'
                            right_open = 0 <= nx < len(board) and 0 <= ny < len(board[0]) and board[nx][ny] == '.'
                            streaks.append((consecutive, left_open, right_open))
                        consecutive = 0
            max_score = 0
            for streak, left, right in streaks:
                opens = left + right
                if streak >= 5:
                    return pattern_weights['win5']
                elif streak == 4:
                    if opens == 2:
                        max_score = max(max_score, pattern_weights['open4'])
                    elif opens == 1:
                        max_score = max(max_score, pattern_weights['halfopen4'])
                elif streak == 3:
                    if opens == 2:
                        max_score = max(max_score, pattern_weights['open3'] * 2)
                    elif opens == 1:
                        max_score = max(max_score, pattern_weights['halfopen3'])
                elif streak == 2:
                    if opens >= 2:
                        max_score = max(max_score, pattern_weights['open2'])
            return max_score

        for x in range(len(board)):
            for y in range(len(board[0])):
                if board[x][y] == player:
                    for dx, dy in [(0,1),(1,0),(1,1),(1,-1)]:
                        score += check_line(x, y, dx, dy, player)
                elif board[x][y] == opponent:
                    for dx, dy in [(0,1),(1,0),(1,1),(1,-1)]:
                        score -= check_line(x, y, dx, dy, opponent)

        for x in range(len(board)):
            for y in range(len(board[0])):
                if board[x][y] == '.':
                    player_potential = 0
                    opponent_potential = 0
                    for dx, dy in [(0,1),(1,0),(1,1),(1,-1)]:
                        for p, mult in [(player, 1), (opponent, -1)]:
                            consecutive = 0
                            opens = 0
                            for dir in [1, -1]:
                                for i in range(1, 6):
                                    nx, ny = x + dx*i*dir, y + dy*i*dir
                                    if 0 <= nx < len(board) and 0 <= ny < len(board[0]):
                                        if board[nx][ny] == p:
                                            consecutive += 1
                                        elif board[nx][ny] == '.':
                                            opens += 1
                                            break
                                        else:
                                            break
                            if consecutive >= 4:
                                score += mult * pattern_weights['open4'] * opens
                            elif consecutive == 3:
                                score += mult * pattern_weights['open3'] * opens
                            elif consecutive == 2:
                                score += mult * pattern_weights['open2'] * opens
        return score

    def minimax(self, board, depth, maximizing, player, opponent):
        key = self.flatten_board_rolling_hash(board)
        if key in memo:
            return memo[key]

        score = self.evaluate(board, player)
        if depth == 0 or abs(score) >= 10_000_000 or not self.get_valid_moves(board):
            memo[key] = score
            return score

        if maximizing:
            maxEval = float('-inf')
            for move in self.get_valid_moves(board):
                if not self.has_neighbor(board, move, opponent):
                    continue
                board[move[0]][move[1]] = player
                eval = self.minimax(board, depth - 1, False, player, opponent)
                board[move[0]][move[1]] = '.'
                maxEval = max(maxEval, eval)
            memo[key] = maxEval if maxEval != float('-inf') else score
            return memo[key]
        else:
            minEval = float('inf')
            for move in self.get_valid_moves(board):
                if not self.has_neighbor(board, move, player):
                    continue
                board[move[0]][move[1]] = opponent
                eval = self.minimax(board, depth - 1, True, player, opponent)
                board[move[0]][move[1]] = '.'
                minEval = min(minEval, eval)
            memo[key] = minEval if minEval != float('inf') else score
            return memo[key]

    def alpha_beta(self, board, depth, alpha, beta, maximizing, player, opponent):
        key = self.flatten_board_rolling_hash(board)
        if key in memo:
            return memo[key]

        score = self.evaluate(board, player)
        if depth == 0 or abs(score) >= 10_000_000 or not self.get_valid_moves(board):
            memo[key] = score
            return score

        if maximizing:
            maxEval = float('-inf')
            for move in self.get_valid_moves(board):
                if not self.has_neighbor(board, move, opponent):
                    continue
                board[move[0]][move[1]] = player
                eval = self.alpha_beta(board, depth - 1, alpha, beta, False, player, opponent)
                board[move[0]][move[1]] = '.'
                maxEval = max(maxEval, eval)
                alpha = max(alpha, eval)
                if beta <= alpha:
                    break
            memo[key] = maxEval if maxEval != float('-inf') else score
            return memo[key]
        else:
            minEval = float('inf')
            for move in self.get_valid_moves(board):
                if not self.has_neighbor(board, move, player):
                    continue
                board[move[0]][move[1]] = opponent
                eval = self.alpha_beta(board, depth - 1, alpha, beta, True, player, opponent)
                board[move[0]][move[1]] = '.'
                minEval = min(minEval, eval)
                beta = min(beta, eval)
                if beta <= alpha:
                    break
            memo[key] = minEval if minEval != float('inf') else score
            return memo[key]

    def get_ai_move(self, board, ai_player, user_player, ai_type="minimax"):
        is_empty = all(cell == '.' for row in board for cell in row)
        if is_empty:
            return (random.randint(0, self.size - 1), random.randint(0, self.size - 1))

        # Check for winning move
        for x in range(len(board)):
            for y in range(len(board[0])):
                if board[x][y] == '.':
                    board[x][y] = ai_player
                    if self.check_win(board, x, y, ai_player):
                        board[x][y] = '.'
                        return (x, y)
                    board[x][y] = '.'

        # Check for blocking opponent's winning move
        critical_moves = []
        for x in range(len(board)):
            for y in range(len(board[0])):
                if board[x][y] == '.':
                    board[x][y] = user_player
                    if self.check_win(board, x, y, user_player):
                        critical_moves.append((x, y))
                    board[x][y] = '.'
        if critical_moves:
            return random.choice(critical_moves)

        # Check for opponent threats
        threat_moves = []
        for x in range(len(board)):
            for y in range(len(board[0])):
                if board[x][y] == '.':
                    board[x][y] = user_player
                    if self.evaluate(board, user_player) >= 2_500_000:
                        threat_moves.append((x, y))
                    board[x][y] = '.'
        if threat_moves:
            return random.choice(threat_moves)

        # Use minimax/alpha-beta for normal move selection
        best_score = -float('inf')
        best_moves = []
        for move in self.get_valid_moves(board):
            if not self.has_neighbor(board, move, user_player):
                continue
            board[move[0]][move[1]] = ai_player
            if ai_type == "minimax":
                score = self.minimax(board, 2, False, ai_player, user_player)
            else:
                score = self.alpha_beta(board, 2, float('-inf'), float('inf'), False, ai_player, user_player)
            board[move[0]][move[1]] = '.'
            if score > best_score:
                best_score = score
                best_moves = [move]
            elif score == best_score:
                best_moves.append(move)

        return random.choice(best_moves) if best_moves else None


def main():
    root = tk.Tk()
    app = GomokuGUI(root)
    root.mainloop()


if __name__ == "__main__":
    main()
