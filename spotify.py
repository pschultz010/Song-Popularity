import spotipy
import csv
import time
from spotipy.oauth2 import SpotifyClientCredentials

print('AND SO IT BEGINS')
start = time.time()

# auth
client_credentials_manager = SpotifyClientCredentials()
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager, requests_timeout=60)

# create csv spreadsheet
fields = ['track_id', 'name', 'artist', 'artist_id', 'artist_popularity', 'artist_followers', 'artist_genres', 'album_id', 'album_type', 'album', 'release_date',
'duration_ms', 'track_number', 'explicit', 'bpm', 'time_signature', 'key', 'mode', 'popularity', 'loudness', 
'energy', 'danceability', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence']
with open('spotify_June2.csv', 'wt') as fp:
    writer = csv.writer(fp, delimiter=",")
    writer.writerow(fields)

def build_tracks(artist):
    tracks = sp.artist_top_tracks(artist['id'])['tracks']
    results = sp.audio_features(tracks = [track['id'] for track in tracks])
    for i, r in enumerate(results):
        if r:
            with open('spotify_June2.csv', 'a') as fp:
                writer = csv.writer(fp, delimiter=",")
                writer.writerow([
                    tracks[i]['id'],
                    tracks[i]['name'],
                    artist['name'],
                    artist['id'],
                    artist['popularity'],
                    artist['followers']['total'],
                    artist['genres'],
                    tracks[i]['album']['id'],
                    tracks[i]['album']['album_type'],
                    tracks[i]['album']['name'],
                    tracks[i]['album']['release_date'],
                    tracks[i]['duration_ms'],
                    tracks[i]['track_number'],
                    tracks[i]['explicit'],
                    r['tempo'],
                    r['time_signature'],
                    r['key'],
                    r['mode'],
                    tracks[i]['popularity'],
                    r['loudness'],
                    r['energy'],
                    r['danceability'],
                    r['speechiness'],
                    r['acousticness'],
                    r['instrumentalness'],
                    r['liveness'],
                    r['valence']
                ])

seen_artist_ids = []
user_playlist = sp.user_playlists('spotify')
i = 0
while user_playlist:
    for playlist in user_playlist['items']:
        for track in sp.playlist(playlist['id'])['tracks']['items']:
            if track['track']:
                for artist in track['track']['artists']:
                    if artist['id'] == None:
                        continue
                    if artist['id'] not in seen_artist_ids:
                        full_artist = sp.artist(artist['id'])
                        build_tracks(full_artist)
                        seen_artist_ids.append(artist['id'])
                        i = i + 1
                        print(i)
    user_playlist = sp.next(user_playlist)

end = time.time()
print('Time Passed: {}'.format(time.strftime('%H:%M:%S', time.gmtime(end - start))))