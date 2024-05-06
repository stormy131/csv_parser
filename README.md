# csv_parser
Haskell term project for "Non-procedural programming"

## Téma projektu
Parser, který umožňuje uživateli provádět dotazy podobné dotazům SQL na data v souborech CSV.

## Popis
Program v jazyce Haskell, který bude pracovat s daty v souborech CSV pomocí dotazů podobných SQL. Program podporuje operátory jako SELECT, WHERE, ORDERBY, JOIN, DROP. Pro tyto účely bude vytvořen Domain Specific Language s použitím vlastních typů pro jednotlivé opeartory. Uživatel bude definovat dotazy pomocí vytvořeného DSL.<br>
Výsledek dotazu pak může být vypsán na STDOUT nebo uložen do výsledného CSV souboru na zadané cestě. Pro čtení a zápis do CSV souborů bude použit built-in modul Haskellu Data.Csv.
