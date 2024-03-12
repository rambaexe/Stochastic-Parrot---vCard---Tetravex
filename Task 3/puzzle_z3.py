import random
from z3 import *

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
    # create an empty puzzle of size x size
    def __init__(self, size):
        self.size = size
        self.puzzle = [[None for _ in range(size)] for _ in range(size)]
        self.initial_solution = [[None for _ in range(size)] for _ in range(size)]

    # create satisfiable puzzle with random tiles
    def Initialise_puzzle(self):
        # set the margin to the random tiles, else set to 0
        for i in range(self.size):
            for j in range(self.size):
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
                # set interior neighbours to be the same as the neighbour at edge
                # up - down, down - up, left - right, right - left
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
        
        # satisfiable puzzle created and stored as initial_solution
        print("Initial solution:")
        self.print_puzzle()
        for i in range(self.size):
            for j in range(self.size):
                self.initial_solution[i][j] = Block(self.puzzle[i][j].up, self.puzzle[i][j].down, self.puzzle[i][j].left, self.puzzle[i][j].right, i, j)

        # shuffle the puzzle based on blocks
        # create list of all blocks
        blocks_list = []
        for i in range(self.size):
            for j in range(self.size):
                blocks_list.append(self.puzzle[i][j])

        # shuffle the list
        random.shuffle(blocks_list)

        # set the shuffled blocks to the puzzle
        for i in range(self.size):
            for j in range(self.size):
                self.puzzle[i][j]= blocks_list.pop()
        
        # shuffled puzzle created
        print("Shuffled puzzle:")
        self.print_puzzle()

    # print puzzle to console in a readable format
    def print_puzzle(self):
        for row in self.puzzle:
            cell_lines = [str(block).split('\n') for block in row]
            for i in range(3):
                print(' | '.join(cell[i] for cell in cell_lines))
            if row != self.puzzle[-1]:
                print('- ' * self.size * 4)
        print("\n\n")
    
    # set puzzle to matrix of blocks, used when solving or generating new puzzles
    def Set_puzzle(self, matrix):
        # matrix block format: up, right, down, left
        for i in range(self.size):
            for j in range(self.size):
                # Block constructor: up, down, left, right -> 0, 2, 3, 1
                self.puzzle[i][j] = Block(matrix[i][j][0], matrix[i][j][2], matrix[i][j][3], matrix[i][j][1], i, j)
    
    # print initial solution to console in a readable format
    def print_puzzle_solution_initial(self):
        print("Initial solution:")
        for row in self.initial_solution:
            cell_lines = [str(block).split('\n') for block in row]
            for i in range(3):
                print(' | '.join(cell[i] for cell in cell_lines))
            if row != self.initial_solution[-1]:
                print('- ' * self.size * 4)
        print("\n\n")

    # puzzle solver
    def solve(self):
        # Create Z3 solver
        puzzle_solver = Solver()

        # variables for solver
        # puzzle_z3 is a 3D array; each inner element is a list of 6 Ints
        puzzle_z3 =[[[] for _ in range(self.size)] for _ in range(self.size)]
        for i in range(self.size):
            for j in range(self.size):
                # each inner element is a block represented by 6 Ints: (tiles:) up, right, down, left, (positions:) i, j
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_up"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_right"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_down"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_left"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_i"))
                puzzle_z3[i][j].append(Int(f"puzzle_{i}_{j}_j"))
               
        # constraints for solver:
        # tiles between 1-9
        tiles_conds = [And(0 <= puzzle_z3[i][j][k], puzzle_z3[i][j][k] <= 9) for i in range(self.size) for j in range(self.size) for k in range(4)]

        # index conditions to be between 0 and size-1 (inclusive)
        index_conds = [And(0 <= puzzle_z3[i][j][k], puzzle_z3[i][j][k] < self.size) for i in range(self.size) for j in range(self.size) for k in range(4,6)]

        # condition neighbours
        # on edge: up - down, down - up, left - right, right - left
        # creating constraints for all inner tiles neigbours
        neighbours_conds = []
        for i in range(self.size):
            for j in range(self.size):
                # up - down
                if i > 0:   
                    neighbours_conds.append(puzzle_z3[i][j][0] == puzzle_z3[i-1][j][2])     
                # down - up
                if i < self.size - 1:
                    neighbours_conds.append(puzzle_z3[i][j][2] == puzzle_z3[i+1][j][0])     
                # left - right
                if j > 0:
                    neighbours_conds.append(puzzle_z3[i][j][3] == puzzle_z3[i][j-1][1])     
                # right - left
                if j < self.size - 1:
                    neighbours_conds.append(puzzle_z3[i][j][1] == puzzle_z3[i][j+1][3])     
        
        # blocks conditions - check that blocks are in the puzzle
        blocks_conds = []
        # loop through the puzzle_z3
        for(i, j) in [(i, j) for i in range(self.size) for j in range(self.size)]:
            # loop through the self.puzzle blocks
            each_block_cond = []
            for row in self.puzzle:
                for block in row:
                    # check if the generated puzzle block is the same as the block in the puzzle
                    each_block_cond.append(And(puzzle_z3[i][j][0] == block.up, puzzle_z3[i][j][1] == block.right, puzzle_z3[i][j][2] == block.down, puzzle_z3[i][j][3] == block.left, puzzle_z3[i][j][4] == i, puzzle_z3[i][j][5] == j))
            # only allow one block to be the same as the block in the puzzle
            blocks_conds.append(Or(each_block_cond))
        
        # add all the constraints to the solver
        puzzle_solver.add(tiles_conds + neighbours_conds + blocks_conds + index_conds)

        # check if there is a solution
        if puzzle_solver.check() == sat:        
            # get the solution
            puzzle_model = puzzle_solver.model()

            # create the solved puzzle as a Puzzle object
            solved_puzzle = [[[] for _ in range(self.size)] for _ in range(self.size)]
            for i in range(self.size):
                for j in range(self.size):
                    solved_puzzle[i][j] = [puzzle_model[puzzle_z3[i][j][k]].as_long() for k in range(4)]
            
            # print the solution
            puzzle_solved = Puzzle(self.size)
            puzzle_solved.Set_puzzle(solved_puzzle)
            print("sat - Solution found")
            puzzle_solved.print_puzzle()
        else:
            # no solution found    
            print("unsat - No solution found")
        
# testing the puzzle solver
def test():
    size = 3
    puzzle = Puzzle(size)
    blocks_matrix = [[[] for _ in range(size)] for _ in range(size)]        # blocks_matrix format: up, right, down, left
    blocks_matrix[0][0] = [4,2,8,9]
    blocks_matrix[0][1] = [2,8,6,3]
    blocks_matrix[0][2] = [2,3,8,2]
    blocks_matrix[1][0] = [5,8,2,7]
    blocks_matrix[1][1] = [5,7,4,8]
    blocks_matrix[1][2] = [8,8,7,0]
    blocks_matrix[2][0] = [6,9,7,1]
    blocks_matrix[2][1] = [8,3,0,8]
    blocks_matrix[2][2] = [7,0,2,9]
    puzzle.Set_puzzle(blocks_matrix)
    print("Test puzzle: ")
    puzzle.print_puzzle()
    puzzle.solve()     

    size = 2                        # size of the puzzle
    puzzle = Puzzle(size)           # create a puzzle object
    puzzle.Initialise_puzzle()      # initialise satisfiable puzzle then shuffle it
    puzzle.solve()                  # solve the puzzle

    size = 3
    puzzle = Puzzle(size)
    puzzle.Initialise_puzzle()
    puzzle.solve()

    size = 4
    puzzle = Puzzle(size)
    puzzle.Initialise_puzzle()
    puzzle.solve()

def main():
    test()
    

if __name__ == "__main__":
    main()