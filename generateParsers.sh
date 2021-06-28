#!/bin/bash
alex src/parsing/Lexer.x
happy src/parsing/Parser.y
mv src/parsing/Lexer.hs src/Lexer.hs
mv src/parsing/Parser.hs src/Parser.hs

alex src/parsing/CommandLexer.x
happy src/parsing/CommandParser.y
mv src/parsing/CommandLexer.hs src/CommandLexer.hs
mv src/parsing/CommandParser.hs src/CommandParser.hs
