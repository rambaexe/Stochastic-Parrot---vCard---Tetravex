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
    close(Stream).

    convert([], _).
convert([H|T], Stream):-
    (   is_list(H)
    ->  (   length(H, 2), H = [Value1, Value2]
        ->  write(Stream, '<tr>'),
            write(Stream, '<td>'),
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