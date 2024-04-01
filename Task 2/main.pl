
/*
    Task 2 - Prolog vCard validator

    Running the validator and HTML convertor: validateandhtml("vcard.txt", "webpage.html").


    Include the rules and facts to knowledge base: consult(main).

*/

:- consult(vcardvalidation).
:- consult(converttohtml).

validateandhtml(InFile, OutFile) :-
    validate_required(InFile).
    %, converthtml(InFile, OutFile).

validate_required(InFile) :-
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
    % print what is in AST
    write(AST3).


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