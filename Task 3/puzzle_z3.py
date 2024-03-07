import random
import time
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
    def __init__(self, size):
        self.size = size
        self.puzzle = [[None for _ in range(size)] for _ in range(size)]
        
        # create puzzle with random tiles
        for i in range(size):
            for j in range(size):
                # set the margin to the random tiles, else set to 0
                up_aux = down_aux = left_aux = right_aux = 0
                
                if i == 0:                              # first row
                    up_aux = random.randint(1, 9)
                if i == size - 1:                     # last row
                    down_aux = random.randint(1, 9)
                if j == 0:                              # first column
                    left_aux = random.randint(1, 9)
                if j == size - 1:                     # last column
                    right_aux = random.randint(1, 9)
                    
                self.puzzle[i][j] = Block(up_aux, down_aux, left_aux, right_aux, i, j) 

        # set neighbours
        for i in range(size):
            for j in range(size):
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
                    if i < size - 1:
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
                    if j < size - 1:
                        if self.puzzle[i][j+1].left == 0:
                            self.puzzle[i][j+1].left = self.puzzle[i][j].right

        self.print_puzzle()

        # shuffle the puzzle
        tiles_list = []
        for i in range(size):
            for j in range(size):
                tiles_list.append(self.puzzle[i][j].up)
                tiles_list.append(self.puzzle[i][j].down)
                tiles_list.append(self.puzzle[i][j].left)
                tiles_list.append(self.puzzle[i][j].right)

        random.shuffle(tiles_list)

        for i in range(size):
            for j in range(size):
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
    

def main():
    #size = int(input("Enter matrix size: "))
    
    size = 3
    puzzle = Puzzle(size)
    


if __name__ == "__main__":
    main()