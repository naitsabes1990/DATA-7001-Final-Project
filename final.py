"""
    checks to make sure that the challenge_set is internally consistent

    usage: python check.py
"""

import json
import pandas as pd
import numpy as np

stats = {
    "tests": 0,
    "errors": 0,
}

required_playlist_fields = ["num_holdouts", "pid", "num_tracks", "tracks", "num_samples"]
optional_playlist_fields = ["name"] + required_playlist_fields

track_fields = set(['pos', 'artist_name', 'artist_uri', 'track_uri', 'track_name',
        'album_uri', 'album_name', 'duration_ms'])

f = open("challenge_set.json")
js = f.read()
challenge_set = json.loads(js)
f.close()


known_ids = set()
unique_tracks = list()
unique_albums = set()
unique_artists = set()

all_playlist=list()
try:
    for a in challenge_set['playlists']:
        for track in a['tracks']:
            data = (a['pid'],track['track_uri'])
            all_playlist.append(data)
except:
    pass

playlist = []
all_track = []

for song in all_playlist:
    pid, track = song
    playlist.append(pid)
    all_track.append(track)

s1=pd.Series(playlist)
s2=pd.Series(all_track)

d = {'pid' : pd.Series(playlist),
    'track' : pd.Series(all_track)}

df = pd.DataFrame(d)

df.to_csv('playlist.csv')
