import lyricsgenius
genius = lyricsgenius.Genius('pJXDiRc6ZJ2pFNfg0rJg4TpuabcxGUMGYH5HJZGfc0OoOp5PUC_Ih1aW6xMYc5qX')
artist = genius.search_artist("zé ramlho", max_songs=3, sort="title")
print(artist.songs)
song = artist.song("Frevo mulher")

print(song.lyrics)