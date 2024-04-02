
/*
    Task 2 - Prolog vCard validator

    Running the validator and HTML convertor: validateandhtml("vcard.txt", "webpage.html").


    Include the rules and facts to knowledge base: consult(main).

*/

:- consult(vcardvalidation).
:- consult(converttohtml).

validateandhtml(InFile, OutFile) :-
    validate_required(InFile, AST),
    converthtml(AST, OutFile).

validate_required(InFile, AST5) :-
    % Read file content
    read_file(InFile, Content),
    AST = [],
    % check if file begins with "BEGIN:VCARD"
    nth0(0, Content, Line),
    check_begin(Line, BeginAST),
    append(AST, [BeginAST], AST1),
    delete(Content, Line, Content1),

    % check if file contains valid version
    nth0(0, Content1, Line1),
    check_version(Line1, VersionAST),
    append(AST1, [VersionAST], AST2),
    delete(Content1, Line1, Content2),

    % check if file ends with "END:VCARD"
    last(Content2, Line2),
    check_end(Line2, EndAST),
    append(AST2, [EndAST], AST3),
    delete(Content2, Line2, Content3),

    write("Required properties satisfied.\n"),

    % check FN property and append it to AST
    nth0(0, Content3, Line3),
    check_FN(Line3, FN_AST),
    append_at_index(2, AST3, FN_AST, AST4),
    delete(Content3, Line3, Content4),

    % check if all required properties are satisfied
    validate_properties(Content4, AST_Properties, AST_Properties_Out),

    % append empty list to AST at index 3
    append_at_index(3, AST4, AST_Properties_Out, AST5),

    % print what is in AST
    write(AST5).


validate_properties([], AST_Properties, AST_Properties) :- 
    write("Properties are valid."),nl.

validate_properties(Content, AST_Properties_In, AST_Properties_Out) :-
    Content = [Line| _],
    % if condition -> all properties are checked here
    (   check_N(Line, AST_property);
        check_BDAY(Line, AST_property);
        check_ANNIVERSARY(Line, AST_property);
        check_GENDER(Line, AST_property);
        check_LANG(Line, AST_property);
        check_ORG(Line, AST_property); 
        check_ADR(Line, AST_property);
        check_TEL(Line, AST_property);
        check_EMAIL(Line, AST_property);
        check_GEO(Line, AST_property);
        check_KEY(Line, AST_property);
        check_TZ(Line, AST_property);
        check_URL(Line, AST_property))
     

    % if property satisfied
    -> 
    append(AST_Properties_In, [AST_property], AST_Properties1),
    delete(Content, Line, Content1),
    validate_properties(Content1, AST_Properties1,AST_Properties_Out);

    % property not satisfied
    nth0(0, Content, Line),
    write("Error: "), write(Line), write("\n"),
    write("Not all properties satisfied.\n"),
    validate_properties([],_,_),
    false.

% read file content and put it in a list
read_file(InFile, Content) :-
    open(InFile, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

% read lines from file
read_lines(Stream, Content) :-
    read_line_to_string(Stream, Line),
    (   Line \== end_of_file
    ->  (   Content = [Line|Content1],
            read_lines(Stream, Content1)
        )
    ;   Content = []
    ).

% append at index 
append_at_index(Index, List, Value, Result) :-
    length(Prefix, Index),
    append(Prefix, Suffix, List),
    append(Prefix, [Value|Suffix], Result).