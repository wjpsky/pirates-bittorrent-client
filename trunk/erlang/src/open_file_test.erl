-module(open_file_test).
-include_lib("eunit/include/eunit.hrl").
-include("torrent_records.hrl").

announce_test() ->	
	A = file_records:toRec(open_file:start("1.torrent")),
	?assertEqual(A#torrent.announce, 
				 "http://torrent.fedoraproject.org:6969/announce").

torrent_files_test() ->
	A = file_records:toRec(open_file:start("1.torrent")),
	?assertEqual(A#torrent.info#torrent_info.files,
				  [{torrent_file,3277086720,"Fedora-13-i386-DVD.iso"},
				   {torrent_file,1598,"Fedora-13-i386-CHECKSUM"}]).	

torrent_name_test() ->
	A = file_records:toRec(open_file:start("1.torrent")),
	?assertEqual(A#torrent.info#torrent_info.name, "Fedora-13-i386-DVD").	


torrent_info_hash_test() ->
	A = file_records:toRec(open_file:start("1.torrent")),
	?assertEqual(A#torrent.info_hash, 
				 <<159,165,60,192,75,22,56,244,28,126,104,67,112,68,107,52,
				   157,95,98,55>>).	

torrent_piece_length_test() ->
	A = file_records:toRec(open_file:start("1.torrent")),
	?assertEqual(A#torrent.info#torrent_info.piece_length, 262144).	

