#!/bin/bash
alex src/parsing/Lexer.x
happy src/parsing/Parser.y
mv src/parsing/Lexer.hs src/Lexer.hs
mv src/parsing/Parser.hs src/Parser.hs
