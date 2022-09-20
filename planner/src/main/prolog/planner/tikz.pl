:- module(tikz, [
	write_tikz/0,
	write_tikz/1,
	build_tikz/0,
	export_tikz/0,
	export_tikz/1
	]).
:- use_module(data).
:- use_module(geometric).
:- use_module(shot).

drop_alphas([],[]).
drop_alphas([H|R], [H|R]) :-
    char_type(H, digit).
drop_alphas([_|R], D) :-
    drop_alphas(R, D).

%% trim_name/2
%% trims atom name for export so it fits into small boxes
%% A : atom such as wood23
%% N : resulting name, e.g. "w23"
trim_name(Atom, Name) :-
	atom_string(Atom, Str),
	string_chars(Str, [C | Chrs]),
	drop_alphas(Chrs, Clean), !,
	string_chars(Name, [C | Clean]).


exportname(Name) :-
	((current_predicate(situation_name/1), situation_name(Name))->
	true;
	Name = 'scene').

%% writes TIKZ drawing commands
%% Out    : stream to write to
%% ID     : label of the object to add, e.g., wood2
%% Color  : fill color
%% Points : list of points
export_poly(Out,ID,Color,Points,CX,CY,A) :-
	format(Out, '\\draw[fill=~a] ', [Color]),
	forall(member([X,Y], Points), format(Out, '(~f,~f) -- ', [0.05*X,-0.05*Y])),
	trim_name(ID, Label),
	format(Out, 'cycle;~n\\node[rotate=~f] at (~f,~f) {\\small ~w};~n', [180.0*(A / pi), 0.05*CX, -0.05*CY, Label]).

%% exports a specific type of shape
export_shape(Out, ID, rect, X, Y, [W, H, A]) :-
	(hasMaterial(ID, M, _, _, _, _); M=lightgray),
	%% draw object
	XRa is (0.5*H),
	YRa is (0.5*W),
	rot_shift(A, X, Y, [[-XRa,-YRa], [-XRa,YRa], [XRa,YRa], [XRa,-YRa]], Points),
	export_poly(Out,ID,M,Points,X,Y,A).

export_shape(Out, ID, unknown, X, Y, _) :-
	hasMaterial(ID, M, _, _, _, _),
	trim_name(ID, Label),
	format(Out, '\\draw[fill=~a] (~f,~f) circle (8pt) node {\\small ~a};~n', [M, X*0.05, -0.05*Y, Label]).

export_shape(Out, ID, poly, X, Y, [_ | Points]) :-
	(hasMaterial(ID, M, _, _, _, _) ; M=lightgray), % hills are of shape type poly but don't have a material assigned
	export_poly(Out,ID,M,Points,X,Y,0).

export_shape(Out, ID, ball, X, Y, [R]) :-
	(hasMaterial(ID, M, _, _, _, _) ; hasColor(ID,M)), % birds are of shape type ball but don't have a material assigned; use red instead
	format(Out,'\\draw[draw,fill=~a] (~f, ~f) circle (~f) node {~w};~n', [M, 0.05*X, -0.05*Y, 0.05*R, ID]).

export_col_shape(Out,ball,X,Y,[R]) :-
	format(Out,'\\draw[draw] (~f, ~f) circle (~f);~n', [0.05*X, -0.05*Y, 0.05*R]).

export_col_shape(Out,poly,_,_,[_ | Points]) :-
	write(Out, '\\draw[draw] '),
	forall(member([X,Y], Points), format(Out, '(~f,~f) -- ', [0.05*X,-0.05*Y])),
	writeln(Out, 'cycle;').

export_col_shape(Out,rect,X,Y,[W, H, A]) :-
	XRa is (0.5*H),
	YRa is (0.5*W),
	rot_shift(A, X, Y, [[-XRa,-YRa], [-XRa,YRa], [XRa,YRa], [XRa,-YRa]], Points),
	export_col_shape(Out, poly, 0, 0, [4 | Points]).

export_col_shape(_,unknown,_,_,_).

draw_parabola(_Out, X, HITX, _A, _B, SX, _SY) :-
	(X + SX) > HITX, !.

draw_parabola(Out, X, HITX, A, B, SX, SY) :-
	(X + SX) =< HITX,
	format(Out, '-- (~f, ~f)', [0.05*(SX+X), -0.05*(SY-A*X*X-B*X)]),
	((X+20+SX > HITX, HITX-SX-X > 2) -> NX is (HITX-SX-0.5) ; NX is X+20),
	draw_parabola(Out, NX, HITX, A, B, SX, SY).


export_parabola(Out, A, B, HITX) :-
	slingshotPivot(SX, SY),
% 	shape(Target, ShapeType, TX, TY, _, ShapeData),
% 	(parabola_crosses_shape(ShapeType, TX, TY, ShapeData, SX, SY, 1000, A, B, HITX) ->
%  true
% 	; (
% 			ground_plane(GY),
% 			parabola_crosses_shape(rect,TX,GY,[2000,1,0], SX, SY, 1000, A, B, HITX)
% 	)),
	format(Out, '\\draw[dotted, very thick] (~f,~f)', [0.05*SX, -0.05*SY]),
	draw_parabola(Out, 10, HITX, A, B, SX, SY),
	writeln(Out, ';').

% export_custom_shot(Out, Shot) :-
	
% 	true.

export_plan(Out, [_, Shot]) :-
%    shape(Target, _, TX, TY, _, _),
%    RA is ((pi*Angle)/180.0),
%    TXScaled is (TX*0.05),
%    TYScaled is (TY*(-0.05)),
%    SX is (TXScaled - 5*cos(RA)),
%    SY is (TYScaled - 5*sin(RA)),
%    EX is (TXScaled - 0.2*cos(RA)),
%    EY is (TYScaled - 0.2*sin(RA)),
	(parabola(_, Shot.uuid, [HITX, HITY], _, A, B) ->
		(
			export_parabola(Out, A, B, HITX),
			format(Out, '\\fill[red] (~f,~f) circle(0.1cm);~n', [0.05*HITX, -0.05*HITY]),
			slingshotPivot(X0, _),
			DXMAX is (HITX-X0),
			shot_obstacles(A, B, DXMAX, Obstacles),
			forall(member([_,HX,HY], Obstacles), format(Out, '\\fill[orange] (~f,~f) circle(0.1cm);~n', [0.05*HX, -0.05*HY]))
		);
		true
	).
%    format(Out, '\\draw[dashed,->] (~f,~f) -- (~f,~f);~n', [SX, SY, EX, EY]).

export_plan(Out, P) :-
	is_dict(P),
	export_plan(Out, [P.target_object, P.shot]).

describe_plan(Out, [Target, Strategy, Confidence, Pigs]) :-
% format(Out,'~w & ~w & ~w & ~2f & $~w$\\\\\n', [Target, Angle, Strategy, Confidence, Pigs]).
	format(Out,'~w & ~w & ~2f & $~w$\\\\\n', [Target, Strategy, Confidence, Pigs]).

describe_plan(Out, [Target, Strategy, Confidence, Pigs, ShotDesc]) :-
% format(Out,'~w & ~w & ~w (~w) & ~2f & $~w$\\\\\n', [Target, Angle, Strategy, ShotDesc, Confidence, Pigs]).
	format(Out,'~w & ~w (~w) & ~2f & $~w$\\\\\n', [Target, Strategy, ShotDesc, Confidence, Pigs]).

describe_plan(Out, [Target, Angle, Strategy, Confidence]) :-
	describe_plan(Out, [Target, Angle, Strategy, Confidence, []]).

describe_plan(Out, P) :-
	is_dict(P),
	describe_plan(Out, [P.target_object, P.strategy, P.confidence, P.reasons]).

table_of_plans(Out, Plans) :-
	((length(Plans, NumberOfPlans), NumberOfPlans =< 5)->
			writeln(Out, '\\matrix (mat) at (6.5,-5.5) [matrix of nodes, row sep=-\\pgflinewidth, draw, rounded corners, nodes={minimum width=1.5cm, minimum height=.5cm, font=\\LARGE}, below right, fill=green!20]{\n');
			(
					(findall(X,shape(_,_,X,_,_,_),List);List=[]), max_list(List,MaxX),
					format(Out, '\\matrix (mat) at (~f,-5.5) [matrix of nodes, row sep=-\\pgflinewidth, draw, rounded corners, nodes={minimum width=1.5cm, minimum height=.5cm}, below right, fill=green!20]{\n', [MaxX*0.05])
			)
	),
	writeln(Out, 'target & strategy & confidence & destroys\\\\ \\hline'),
	forall(member(P, Plans), describe_plan(Out, P)),
	writeln(Out, '};').


%%
%% Writes a latex file displaying the current scene, i.e., all objects mentioned as shape/6 predicates.
%% Also indicates where shots are planned according to list of Plans in the format
%% [[target, angle, strategy, confidence, pigs_affected] | ... ]
%%
write_tikz(Plans) :-
	%% open file, write latex header
	exportname(SceneName),
	string_concat(SceneName, '.tex', TexName),
	open(TexName, write, Out),
	(\+ getenv('CONVERT_ENABLE', true) ->
			writeln(Out, '\\documentclass[tikz]{standalone}');
			(current_prolog_flag(windows, true) ->
				writeln(Out, '\\documentclass[tikz,convert={outext=.jpg,size=1500,{convertexe={magick.exe}}}]{standalone}');
				writeln(Out, '\\documentclass[tikz,convert={outext=.jpg,size=1500}]{standalone}')
			)
	),
	writeln(Out, '\\usetikzlibrary{matrix,backgrounds}'),
	writeln(Out, '\\definecolor{ice}{rgb}{0.6,0.7,1.0}'),
	writeln(Out, '\\definecolor{stone}{rgb}{0.5,0.5,0.5}'),
	writeln(Out, '\\definecolor{wood}{rgb}{0.9,0.6,0.1}'),
	writeln(Out, '\\definecolor{pork}{rgb}{0.1,1.0,0.1}'),
	writeln(Out, '\\definecolor{tnt}{rgb}{0.9,0.9,0.0}'),
	writeln(Out, '\\begin{document}'),
	writeln(Out, '\\begin{tikzpicture}[background rectangle/.style={fill=white!45}, show background rectangle]'),
	((length(Plans, NumberOfPlans), NumberOfPlans =< 5)->
			writeln(Out, '\\clip (6,-20) rectangle(40, -5);');
			true
	),

	%% draw all objects in scene by  executing goal export_shape/6 defined above for all shapes
	forall(col_shape(_,Type,X,Y,_,Params), export_col_shape(Out,Type,X,Y,Params)),
	forall(shape(ID,Type,X,Y,_,Params), export_shape(Out,ID,Type,X,Y,Params)),

	%% draw slingshot
	slingshotPivot(SX,SY),
	format(Out, '\\draw (~f,~f) -- +(0:1cm) -- +(180:1cm) -- +(0:0) -- +(90:1cm) -- +(270:1cm);~n', [0.05*SX, -0.05*SY]),

	%% draw plans
	forall(member(P, Plans), export_plan(Out, P)),

	%% add table of plans
	(Plans\=[] ->
		table_of_plans(Out, Plans);
		forall(
			isHittable(Object, UUID),
			(
				parabola(Object,UUID,[HITX,HITY],_,A,B),
				format(Out, '\\draw[dotted, very thick] (~f,~f)', [0.05*SX, -0.05*SY]),
				draw_parabola(Out, 10, HITX, A, B, SX, SY),
				writeln(Out, ';'),
				format(Out, '\\fill[red] (~f,~f) circle(0.1cm)', [0.05*HITX, -0.05*HITY]),
				writeln(Out, ';')
			)
		)
	),

	%%  finish picture
	writeln(Out, '\\end{tikzpicture}'),

	%% finish document, close file, and typeset
	writeln(Out, '\\end{document}'),
	close(Out).

write_tikz :-
	write_tikz([]).

build_tikz :-
	exportname(SceneName),
	string_concat(SceneName, '.tex', TexName),
	string_concat(SceneName, '.pdf', PdfName),
	string_concat('pdflatex -halt-on-error -shell-escape ', TexName, PdfLatex),
	catch((shell(PdfLatex),
        (\+ getenv('EXPORT_DISPLAY_DISABLE', true) ->
                (current_prolog_flag(windows, true)
                -> win_shell(open,PdfName);
                        (current_prolog_flag(apple, true)
                        -> (string_concat('open ', PdfName, AppleOpen),
                                shell(AppleOpen));
                             (string_concat('xdg-open ', PdfName, LinuxOpen),
                                shell(LinuxOpen))
                        )
                );
                true
        )),
        Error,
        print_message(error,Error)
     ).

export_tikz(Plans) :-
	write_tikz(Plans),
	build_tikz.

export_tikz :-
	export_tikz([]).
