
%% length is an int / path is a list of strings
-record(torrent_file,{length,path}).

%% files is a list of torrent_file record / name is string / piece_length is int
%% pieces is a list of tuples {piece_number,piece_hashcode}
-record(torrent_info,{files,name,piece_length,pieces}).

%% announce is a string / info is a torrent_info record
-record(torrent,{announce,info,info_hash}).
