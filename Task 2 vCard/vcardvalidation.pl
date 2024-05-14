% check if property appears at most once - cardinality check
check_at_most_once(Property, Content) :-
    findall(X, (member(X, Content), re_match(Property, X)), Matches),   % take each line from list; re_match with property; store in Matches
    length(Matches, Count),                                             % count number of matches                           
    (Count =< 1                                                         % cardinality check: at most once
    ->  true
    ;   write("Error: "), write([Property]), write(" appears more than once.\n"),
        throw("Error encountered.")
    ).

% checks happen here
check_at_most_once_properties([]).
check_at_most_once_properties(Content_inserted) :-
    % N property should appear at most once
    check_at_most_once("N:[^;]*;[^;]*;[^;]*;[^;]*;[^;]*", Content_inserted), 
    % BDAY property should appear at most once
    (   check_at_most_once("BDAY:\\d{8}", Content_inserted)
        ; check_at_most_once("BDAY:--\\d{4}", Content_inserted)
        ; check_at_most_once("BDAY:\\d{4}-\\d{2}-\\d{2}", Content_inserted)
        ; check_at_most_once("BDAY;\\d{8}T\\d{6}Z", Content_inserted)
        ; check_at_most_once("BDAY;VALUE=text:", Content_inserted)),
    % ANNIVERSARY property should appear at most once
    (   check_at_most_once("ANNIVERSARY:\\d{8}T\\d{4}-\\d{4}", Content_inserted)
        ; check_at_most_once("ANNIVERSARY:\\d{8}$", Content_inserted)),
    % GENDER property should appear at most once
    (   check_at_most_once("GENDER:[MFONU]+;?.*", Content_inserted)
        ; check_at_most_once("GENDER:;\\S+", Content_inserted)).


/*
    For each property, check if it is valid.
    If property is valid, split the line into key and value and store in AST.
    If property is not valid, return false.
*/


% check if property is BEGIN
check_begin(Line, AST):-
    re_match("BEGIN:VCARD", Line) ->
    write("BEGIN:VCARD found.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST = [Key, Value],
    true;
    write("Error: No BEGIN:VCARD. vCard not valid."), false.

% check if property is END
check_end(Line, AST):-
    re_match("END:VCARD", Line) -> 
    write("END:VCARD found.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST = [Key, Value],
    true;
    write("Error: No END:VCARD. vCard not valid."), false.

% check if property is VERSION
check_version(Line, AST):-
    re_match("VERSION:4.0", Line) -> 
    write("VERSION:4.0 found.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST = [Key, Value],
    true;
    write("Error: No VERSION:4.0. vCard not valid."), false.

% check if property is FN
check_FN(Line, AST_property):-
    re_match("FN:.+", Line) -> 
    write("FN: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is N
check_N(Line, AST_property):-
    re_match("N:[^;]*;[^;]*;[^;]*;[^;]*;[^;]*", Line) -> 
    write("N: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is BDAY
check_BDAY(Line, AST_property):-
    (   re_match("BDAY:\\d{8}", Line);
        re_match("BDAY:--\\d{4}", Line);
        re_match("BDAY:\\d{4}-\\d{2}-\\d{2}", Line);
        re_match("BDAY;\\d{8}T\\d{6}Z", Line);
        re_match("BDAY;VALUE=text:.+", Line))
    -> 
    write("BDAY: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is ANNIVERSARY
check_ANNIVERSARY(Line, AST_property):-
    (   re_match("ANNIVERSARY:\\d{8}T\\d{4}-\\d{4}", Line);
        re_match("ANNIVERSARY:\\d{8}$", Line))
    -> 
    write("ANNIVERSARY: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is GENDER
check_GENDER(Line, AST_property):-
    (re_match("GENDER:[MFONU]+;?.*", Line);
    re_match("GENDER:;\\S+", Line))
    -> 
    write("Gender: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is LANG
check_LANG(Line, AST_property):-
    (re_match("LANG;TYPE=.+;PREF=\\d+:.+", Line);
    re_match("LANG;TYPE=.+:.+", Line);
    re_match("LANG;PREF=\\d+:.+", Line)) 
    -> 
    write("LANG: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is ORG
check_ORG(Line, AST_property):-
    (re_match("ORG:.+", Line);
    re_match("ORG;TYPE=.+:.+", Line))
    -> 
    write("ORG: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is ADR
check_ADR(Line, AST_property):-
    (re_match("ADR;TYPE=.+:.*;.*;.*;.*;.*;.*", Line))
    -> 
    write("ADR: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is TEL
check_TEL(Line, AST_property):-
    (re_match("TEL;VALUE=.+;TYPE=.+:tel:\\+?\\d+(-\\d+)*(;ext=\\d+)?$", Line);
    re_match("TEL;VALUE=.+;PREF=\\d+;TYPE=.+:tel:\\+?\\d+(-\\d+)*(;ext=\\d+)?$", Line);
    re_match("TEL;VALUE=.+;TYPE=.+;PREF=\\d+:tel:\\+?\\d+(-\\d+)*(;ext=\\d+)?$", Line))
    -> 
    write("TEL: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value, Value2]),
    AST_property = [Key, Value:Value2],
    true;
    false.

% check if property is EMAIL
check_EMAIL(Line, AST_property):-
    (re_match("EMAIL;TYPE=.+:.+@.+\\..+", Line);
    re_match("EMAIL;PREF=\\d+:.+@.+\\..+", Line))
    -> 
    write("EMAIL: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is GEO
check_GEO(Line, AST_property):-
    (re_match("GEO:geo:-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?", Line))
    ->
    write("GEO: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    re_match("GEO;TYPE=.+:geo:-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?", Line)
    -> 
    write("GEO: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value, Value2]),
    AST_property = [Key, Value:Value2],
    true;
    false.

% check if property is KEY
check_KEY(Line, AST_property):-
    (re_match("KEY:.+", Line);
    re_match("KEY;MEDIATYPE=.+:.+", Line);
    re_match("KEY;TYPE=.+;VALUE=.+:.+", Line))
    -> 
    write("KEY: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value, Value2]),
    AST_property = [Key, Value:Value2],
    true;
    false.

% check if property is TZ 
check_TZ(Line, AST_property):-
    (re_match("TZ:-?\\d{4}", Line);
    re_match("TZ:.+/.*", Line))
    -> 
    write("TZ: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value]),
    AST_property = [Key, Value],
    true;
    false.

% check if property is URL
check_URL(Line, AST_property):-
    (re_match("URL:http.+", Line);
    re_match("URL;TYPE=.+:http.+", Line))
    -> 
    write("URL: found and valid.\n"),
    split_string(Line, ":", "", [Key, Value, Value2]),
    AST_property = [Key, Value:Value2],
    true;
    false.

