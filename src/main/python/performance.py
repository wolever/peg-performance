#!/usr/bin/env python2

# 
# performance.py
# 
# Copyright (c) 2010, Jonathan Fuerth
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * Neither the name of Jonathan Fuerth nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

from collections import namedtuple

class Move(namedtuple("Move", "fromh jumped to")):
    def __str__(self):
        return str(self.fromh) + " -> " + str(self.jumped) + " -> " + str(self.to)

class Coordinate(namedtuple("Coordinate", "row hole")):
    def legalMoves(self, rowCount, occupiedHoles, __moves=[(2, 0), (-2, 0), (2, 2), (-2, -2), (0, 2), (0, -2)]):
        row = self.row
        hole = self.hole

        for rOff, hOff in __moves:
            newRow = row + rOff
            newHole = hole + hOff
            if newRow < 1 or newHole < 1 or newRow < newHole or newRow > rowCount:
                continue
            t = (newRow, newHole)
            if t in occupiedHoles:
                continue
            j = (row + rOff // 2, hole + hOff // 2)
            if j not in occupiedHoles:
                continue
            yield Move(
                self,
                Coordinate(*j),
                Coordinate(*t),
            )

    def __str__(self):
        return "r" + str(self.row) + "h" + str(self.hole)


class GameState(object):

    def __init__(self, rows, emptyHole, initialState=None, applyMe=None):

        if initialState != None:
            # top-secret constructor overload for applying a move
            self.rowCount = initialState.rowCount
            self.occupiedHoles = set()
            self.occupiedHoles.update(initialState.occupiedHoles)

            # Note to those comparing this implementation to the others:
            # List.remove() raises ValueError if thr requested item is
            # not present, so the explicit errors are not raised here.
            # The self-checking nature of this method is still intact.

            self.occupiedHoles.remove(applyMe.fromh)
            self.occupiedHoles.remove(applyMe.jumped)

            if applyMe.to in self.occupiedHoles:
                raise RuntimeError, "Move is not consistent with game state: 'to' hole was occupied."

            if (applyMe.to.row > self.rowCount or applyMe.to.row < 1):
                raise RuntimeError, "Move is not legal because the 'to' hole does not exist: " + str(applyMe.to)

            self.occupiedHoles.add(applyMe.to)

        else:
            # normal constructor that sets up board
            self.rowCount = rows;
            self.occupiedHoles = set()
            for row in range(1, rows + 1):
                for hole in range(1, row + 1):
                    peg = Coordinate(row, hole)
                    if (not peg == emptyHole):
                        self.occupiedHoles.add(peg)

    def legalMoves(self):
        for c in self.occupiedHoles:
            for m in c.legalMoves(self.rowCount, self.occupiedHoles):
                yield m
    
    def applyMove(self, move):
        return GameState(None, None, self, move)

    def pegsRemaining(self):
        return len(self.occupiedHoles)

    def __str__(self):
        sb = []
        sb.append("Game with " + str(self.pegsRemaining()) + " pegs:\n")
        for row in range(1, self.rowCount + 1):
            indent = self.rowCount - row
            for i in range(0, indent):
                sb.append(" ")
            for hole in range(1, row + 1):
                if Coordinate(row, hole) in self.occupiedHoles:
                    sb.append(" *")
                else:
                    sb.append(" O")
            sb.append("\n")
        return "".join(sb)


gamesPlayed = 0
solutions = []
    
def search(gs, moveStack):
    global gamesPlayed
    global solutions
    
    if (gs.pegsRemaining() == 1):
        #print("Found a winning sequence. Final state:")
        #print(gs);

        solutionCopy = []
        solutionCopy.extend(moveStack)
        solutions.append(solutionCopy)
        
        gamesPlayed += 1
        return

    for m in gs.legalMoves():
        nextState = gs.applyMove(m)
        moveStack.append(m)
        search(nextState, moveStack)
        moveStack.pop()

    gamesPlayed += 1

from time import time

startTime = time()
gs = GameState(5, Coordinate(3, 2))
search(gs, [])
endTime = time()

print "Games played:    %6d" % (gamesPlayed)
print "Solutions found: %6d" % (len(solutions))
print "Time elapsed:    %6dms" % ((endTime - startTime) * 1000)
