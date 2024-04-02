/*
    Task 2 - Prolog vCard validator

    Include the rules and facts to knowledge base: consult(main).
    Running the validator and HTML convertor: validateandhtml("vcard.txt", "webpage.html").

    Cardinality rules (for Version 4.0):
    1 - exactly one must be present: BEGIN, VERSION, END, FN
    *1 - exactly one may be present: N, BDAY, ANNIVERSARY, GENDER
    1* - one or more must be present: -
    * - one or more may be present: LANG, ORG, ADR, TEL, EMAIL, GEO, KEY, TZ, URL

*/

:- consult(vcardvalidation).
:- consult(converttohtml).

validateandhtml(InFile, OutFile) :-
    validate_required(InFile, AST),
    converthtml(AST, OutFile).

validate_required(InFile, AST5) :-
    % Read file content
    read_file(InFile, Content),
    
    % initialise AST
    AST = [],

    % check properties of cardinality 1 (required properties)
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

    % check FN property 
    nth0(0, Content3, Line3),
    check_FN(Line3, FN_AST),
    append_at_index(2, AST3, FN_AST, AST4),
    delete(Content3, Line3, Content4),

    write("Required properties satisfied.\n"),

    % check cardinality for *1 properties
    check_at_most_once_properties(Content4),

    % check if all properties are satisfied for *1 or *
    AST_Properties = [],
    validate_properties(Content4, AST_Properties, AST_Properties_Out),
    write(AST_Properties_Out), nl,

    % append the properties found to AST at index 3
    append_at_index(3, AST4, [AST_Properties_Out], AST5),

    % print what is in AST
    write(AST5), nl.


% validate properties - *1 or *
% check if property (given in Line from Content) is valid
% add valid ones to AST_Properties; otherwise throw error
validate_properties([], AST_Properties, AST_Properties) :- 
    write("Properties are valid."), nl.

validate_properties(Content, AST_Properties, Final_AST_Properties) :-
    Content = [Line|_], 
    (
        check_N(Line, AST_property);
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
        check_URL(Line, AST_property)
    ),
    append(AST_Properties, [AST_property], Updated_AST_Properties),
    delete(Content, Line, Content1),
    validate_properties(Content1, Updated_AST_Properties, Final_AST_Properties).

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

