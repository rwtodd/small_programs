% vim: set fileytpe=prolog :
% reading the FAT filesystem for
% disk images

read_word(Strm, Word) :-
    get_byte(Strm, Low),
    get_byte(Strm, High),
    Word is (High << 8) + Low.

read_str(Strm, N, [S|SS]) :-
    N > 0,
    N1 is N - 1,
    get_byte(Strm,S),
    read_str(Strm,N1,SS).
read_str(_,0,[]).

:- dynamic(header/2).

read_header(Strm) :-
    retractall(header/2),
    seek(Strm, 0x03, bof, _),
    read_str(Strm,8,CSI),   % Creating System ID
    assert(header(csi,CSI)),
    read_word(Strm,SS),     % SS sector size
    assert(header(ss,SS)),
    get_byte(Strm,SC),      % SC sectors per cluster
    assert(header(sc,SC)),
    read_word(Strm,RSC),    % RSC reserved sector count
    assert(header(rsc,RSC)),
    get_byte(Strm,FN),      % FN number of FATs
    assert(header(fn,FN)),
    read_word(Strm,RDE),    % RDE number of root directory entries
    assert(header(rde,RDE)),
    read_word(Strm,TS),     % TS total sectors
    assert(header(ts,TS)),
    get_byte(Strm,MedId),   % MedId = Medium Identifier
    assert(header(medid,MedId)),
    read_word(Strm,SF),     % SF sectors per FAT
    assert(header(sf,SF)),
    read_word(Strm,SPT),    % SPT sectors per track
    assert(header(spt,SPT)),
    read_word(Strm,NOS),    % NOS number of sides
    assert(header(nos,NOS)).

skip_to_sector(Strm,N) :-
    header(ss,Size),
    Loc is N * Size,
    seek(Strm,Loc,bof,_).


calculate_rootdir :-   % root directory sector
    header(fn,FN), % number of fats
    header(sf,SF),  % sectors per fat
    header(rsc,RSC), % reserved sectors
    RootDir is (FN*SF) + RSC,
    assert(header(rootdir,RootDir)).

calculate_ssa :-      % size of the system area in sectors
    header(rootdir,RootDir),
    header(rde,RDE),
    header(ss,SS),
    SSA is RootDir + ceiling((32*RDE)/SS),
    assert(header(ssa,SSA)).

calculate_derived :-
    calculate_rootdir,
    calculate_ssa.

cluster_lsn(CN,LSN) :-
    header(ssa,SSA),
    header(sc,SC),
    LSN is SSA + (CN - 2)*SC.



main :-
    open("disk.img", read, File, [type(binary)]),
    read_header(File),
    calculate_derived,
    close(File).
