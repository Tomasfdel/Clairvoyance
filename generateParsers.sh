#!/bin/bash
alex src/parsing_sources/FileLexer.x
happy src/parsing_sources/FileParser.y
mv src/parsing_sources/FileLexer.hs src/FileParser/Lexer.hs
mv src/parsing_sources/FileParser.hs src/FileParser/Parser.hs

alex src/parsing_sources/CommandLexer.x
happy src/parsing_sources/CommandParser.y
mv src/parsing_sources/CommandLexer.hs src/Commands/Lexer.hs
mv src/parsing_sources/CommandParser.hs src/Commands/Parser.hs
