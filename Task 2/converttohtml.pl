% convert AST to HTML
% create HTML file format with a table where AST is displayed
converthtml(AST, OutFile):-
    open(OutFile, write, Stream),
    write(Stream, '<!DOCTYPE html>'),
    write(Stream, '<html>'),
    write(Stream, '<head>'),
    write(Stream, '<title>Prolog AST</title>'),
    write(Stream, '<style>'),
    write(Stream, 'body {font-family: Arial, sans-serif;}'),
    write(Stream, 'table {width: auto; margin: auto;}'),
    write(Stream, 'td {padding: 10px; border: 1px solid #ddd; font-size: 0.8em;}'),
    write(Stream, '</style>'),
    write(Stream, '</head>'),
    write(Stream, '<body>'),
    write(Stream, '<table>'),
    convert(AST, Stream),
    write(Stream, '</table>'),
    write(Stream, '</body>'),
    write(Stream, '</html>'),
    close(Stream),
    write("HTML created successfully!"), nl, nl, nl.    

% convert AST to HTML table format
% when property is a list, create a new row with the values
% otherwise, create a new row with the property value
% column on the left is the key (name of the property)
% column on the right is the value of the property from the text file
convert([], _).
convert([H|T], Stream):-
    (   is_list(H)
    ->  (   length(H, 2), H = [Value1, Value2]
        ->  write(Stream, '<tr>'),      % add row
            write(Stream, '<td>'),      % add column
            write(Stream, Value1),
            write(Stream, '</td>'),
            write(Stream, '<td>'),
            write(Stream, Value2),
            write(Stream, '</td>'),
            write(Stream, '</tr>')
        ;   convert(H, Stream)
        )
    ;   (   T = []
        ->  write(Stream, H)
        ;   write(Stream, '<tr>'),
            write(Stream, '<td>'),
            write(Stream, H),
            write(Stream, '</td>'),
            write(Stream, '</tr>')
        )
    ),
    convert(T, Stream).