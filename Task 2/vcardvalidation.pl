library(pcre).

check_begin(Line, AST):-
    re_match("BEGIN:VCARD", Line) ->
    split_string(Line, ":", "", [Key, Value]),
    AST = [Key, Value],
    true;
    write("Error: No BEGIN:VCARD. vCard not valid."), false.

check_end(Line, AST):-
    re_match("END:VCARD", Line) -> 
    split_string(Line, ":", "", [Key, Value]),
    AST = [Key, Value],
    true;
    write("Error: No END:VCARD. vCard not valid."), false.

check_version(Line, AST):-
    re_match("VERSION:4.0", Line) -> 
    split_string(Line, ":", "", [Key, Value]),
    AST = [Key, Value],
    true;
    write("Error: No VERSION:4.0. vCard not valid."), false.

