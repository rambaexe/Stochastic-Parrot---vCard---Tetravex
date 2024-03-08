import random
import time
from z3 import *
from z3 import Int
from collections import defaultdict

class Block:
    def __init__(self, up, down, left, right, position_row, position_col):
        self.up = up
        self.down = down
        self.left = left
        self.right = right
        self.row = position_row
        self.col = position_col

    def __str__(self):
        return f"  {self.up}  \n{self.left}   {self.right}\n  {self.down}  "
    
class Puzzle:
    def __init__(self, size):
        self.size = size
        self.puzzle = [[None for _ in range(size)] for _ in range(size)]

    def Initialise_puzzle(self):    
        # create puzzle with random tiles
        for i in range(self.size):
            for j in range(self.size):
                # set the margin to the random tiles, else set to 0
                up_aux = down_aux = left_aux = right_aux = 0
                
                if i == 0:                              # first row
                    up_aux = random.randint(1, 9)
                if i == self.size - 1:                     # last row
                    down_aux = random.randint(1, 9)
                if j == 0:                              # first column
                    left_aux = random.randint(1, 9)
                if j == self.size - 1:                     # last column
                    right_aux = random.randint(1, 9)
                    
                self.puzzle[i][j] = Block(up_aux, down_aux, left_aux, right_aux, i, j) 

        # set neighbours
        for i in range(self.size):
            for j in range(self.size):
                # set interior neighbours to be the same as the margin of the neighbour
                
                if self.puzzle[i][j].up == 0:
                    self.puzzle[i][j].up = random.randint(1, 9)
                    # neighbours: self.puzzle[i-1][j].down; self.puzzle[i][j].up
                    if i > 0:
                        if self.puzzle[i-1][j].down == 0:
                            self.puzzle[i-1][j].down = self.puzzle[i][j].up

                if self.puzzle[i][j].down == 0:
                    self.puzzle[i][j].down = random.randint(1, 9)
                    # neighbours: self.puzzle[i+1][j].up; self.puzzle[i][j].down
                    if i < self.size - 1:
                        if self.puzzle[i+1][j].up == 0:
                            self.puzzle[i+1][j].up = self.puzzle[i][j].down

                if self.puzzle[i][j].left == 0:
                    self.puzzle[i][j].left = random.randint(1, 9)
                    # neighbours: self.puzzle[i][j-1].right; self.puzzle[i][j].left
                    if j > 0:
                        if self.puzzle[i][j-1].right == 0:
                            self.puzzle[i][j-1].right = self.puzzle[i][j].left

                if self.puzzle[i][j].right == 0:
                    self.puzzle[i][j].right = random.randint(1, 9)
                    # neighbours: self.puzzle[i][j+1].left; self.puzzle[i][j].right
                    if j < self.size - 1:
                        if self.puzzle[i][j+1].left == 0:
                            self.puzzle[i][j+1].left = self.puzzle[i][j].right

        self.print_puzzle()

        # shuffle the puzzle
        tiles_list = []
        for i in range(self.size):
            for j in range(self.size):
                tiles_list.append(self.puzzle[i][j].up)
                tiles_list.append(self.puzzle[i][j].down)
                tiles_list.append(self.puzzle[i][j].left)
                tiles_list.append(self.puzzle[i][j].right)

        random.shuffle(tiles_list)

        for i in range(self.size):
            for j in range(self.size):
                self.puzzle[i][j].up = tiles_list.pop()
                self.puzzle[i][j].down = tiles_list.pop()
                self.puzzle[i][j].left = tiles_list.pop()
                self.puzzle[i][j].right = tiles_list.pop()

        self.print_puzzle()

    def print_puzzle(self):
        for row in self.puzzle:
            cell_lines = [str(block).split('\n') for block in row]
            for i in range(3):
                print(' | '.join(cell[i] for cell in cell_lines))
            if row != self.puzzle[-1]:
                print('- ' * self.size * 4)
        print("\n\n")
    
    def Set_puzzle(self, matrix):
        for i in range(self.size):
            for j in range(self.size):
                # Block constructor: up, down, left, right -> 0, 2, 3, 1
                self.puzzle[i][j] = Block(matrix[i][j][0], matrix[i][j][2], matrix[i][j][3], matrix[i][j][1], i, j)

    def solve(self):
        # Create Z3 solver
        puzzle_solver = Solver()

        # variables for solver
        puzzle_z3 =[[[] for _ in range(self.size)] for _ in range(self.size)]

        numbers = []
        for i in range(self.size):
            for j in range(self.size):
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_up"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_right"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_down"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_left"))
            
                numbers.append(self.puzzle[i][j].up)
                numbers.append(self.puzzle[i][j].right)
                numbers.append(self.puzzle[i][j].down)
                numbers.append(self.puzzle[i][j].left)    
               

        # constraints for solver:
        # tiles between 1-9
        tiles_conds = [And(1 <= puzzle_z3[i][j][k], puzzle_z3[i][j][k] <= 9) for i in range(self.size) for j in range(self.size) for k in range(4)]

        # condition neighbours
        neighbours_conds = []
        for i in range(self.size):
            for j in range(self.size):
                if i > 0:   
                    neighbours_conds.append(puzzle_z3[i][j][0] == puzzle_z3[i-1][j][2])     # up - down
                if i < self.size - 1:
                    neighbours_conds.append(puzzle_z3[i][j][2] == puzzle_z3[i+1][j][0])     # down - up
                if j > 0:
                    neighbours_conds.append(puzzle_z3[i][j][3] == puzzle_z3[i][j-1][1])     # left - right
                if j < self.size - 1:
                    neighbours_conds.append(puzzle_z3[i][j][1] == puzzle_z3[i][j+1][3])     # right - left
        
        # conditions that each number must be used from self.puzzle
        numbers_conds = []        
        
        # add all the constraints to the solver
        puzzle_solver.add(tiles_conds + neighbours_conds + numbers_conds)

        # check if there is a solution
        if puzzle_solver.check() == sat:        
            # get the solution
            puzzle_model = puzzle_solver.model()

            # create the solved puzzle as a Puzzle object
            solved_puzzle = [[[] for _ in range(self.size)] for _ in range(self.size)]
            for i in range(self.size):
                for j in range(self.size):
                    solved_puzzle[i][j] = [puzzle_model[puzzle_z3[i][j][k]].as_long() for k in range(4)]
            
            puzzle_solved = Puzzle(self.size)
            puzzle_solved.Set_puzzle(solved_puzzle)
            print("sat - Solution found")
            puzzle_solved.print_puzzle()

        else:    
            print("unsat - No solution found")
        

def main():
    #size = int(input("Enter matrix size: "))
    
    size = 4
    puzzle = Puzzle(size)
    puzzle.Initialise_puzzle()
    
    solution = puzzle.solve()

if __name__ == "__main__":
    main()