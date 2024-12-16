import matplotlib.pyplot as plt
import numpy as np
import re
import json
import time
import math
from spotify_api_miner import *

# Print functions
def print_query():
    query = """
    Type "exit" to leave program (and to save added artists for future use)
    Type 0 to show list of artists
    Type 1 to plot the release history of an artist.
    Type 2 to learn about artist's wikipedia data and analyze their lyrics.
    Type 3 to compare an artist to another artist or the charts.
    Type x to add an artist (requires internet connection and takes up to 50 seconds)
"""
    print(query)

def print_artists(artists):
    print("-----------------------------------------------")
    print(f"{'Artist Nr.':<10}{'Artist Name':>30}")
    print("-----------------------------------------------")
    for nr, name in artists.items():
        print(f"{nr:<10}{name[0]:>30}")
    print("-----------------------------------------------")

def print_wiki_info_text(artist):

    wiki_info_dict = retrieve_wikipage_info(artist)
    view_data = retrieve_wikipage_views_month(artist)
    views_info = analyze_wikipage_views(view_data)

    wikipedia_template = f"""
=====================================================================
Wikipedia Page Info about {artist}

Open Wiki Page: {wiki_info_dict["wiki_url"]}

Views in the last 30 days: {views_info[0]:>7}
Average views per day: {views_info[1]:>11}

Total number of words: {wiki_info_dict["total_words"]:>11}
An average reader needs {wiki_info_dict["reading_time"]} minutes to read the text (280 WPM).
=====================================================================
"""
    print(wikipedia_template)

# Functions to read files
def read_artists():
    try:
        with open("resources/artists.json", 'r')as file:
            artists = json.load(file)
        if '1' in artists.keys(): 
            return artists
    except: 
        return None

def read_top(artist_nr):  
    try:
        filename = "resources/top_"+artist_nr+".json"
        with open(filename, 'r') as file:
            top_data = json.load(file)
        return top_data
    except:
        print("A file is missing. Add artist again to retrieve their top songs.")

def read_charts():
    try: 
        with open("resources/charts.json", 'r') as file:
            charts = json.load(file)
        return charts
    except: 
        try:
            charts = retrieve_charts()
            return charts
        except: 
            print("Charts could not be accessed. Check your internet connection")
            return None

def read_charts_features():
    try:    
        with open("resources/features_charts.json", 'r')as file:
            audio_features = json.load(file)
        return audio_features
    except:
        try:
            charts = read_charts()
            id_list = []
            for song in charts['tracks']['items'] :
                id_list.append(song['track']['id'])
            retrieve_audio_features(id_list, "charts")
            return audio_features
        except: 
            print("Charts could not be accessed. Check your internet connection")
            return None

def read_features(artist_nr):       
    filename = "resources/features_"+artist_nr+".json"
    with open(filename, 'r')as file:
        audio_features = json.load(file)
    return audio_features
 
def read_release_history(artist_nr):    
    filename = "resources/release_history_"+artist_nr+".json"
    with open(filename, 'r')as file:
        release_history = json.load(file)
    return release_history

# Plotting functions
def plot_release_history(dates, name) :
    dates.sort()
    xpoints = []
    years = list(range(int(dates[0]),int(dates[-1])))
    xpoints.extend(years)
    ypoints = [0] * len(xpoints)
    for i in range(len(dates)) :
        for j in range(len(xpoints)) :
            if int(dates[i]) == xpoints[j] :
                ypoints[j] += 1

    plt.plot(np.asarray(xpoints), np.asarray(ypoints))
    plt.title('Release history')
    plt.xlabel('Year')
    plt.ylabel('Release count')
    filename = f"{name.lower()}_release_history.png"
    plt.savefig("./resources/"+filename)
    plt.show()

def plot_pronouns(buckets, track):
    plt.bar(["I", "You", "They"], buckets)
    filename = f"{track}_lyrics.png"
    plt.savefig("resources/" + filename)
    plt.show()

# Analysis functions
def get_release_dates_of_artist(data) :
    releases = []
    for album in range(len(data['items'])) :
        date = data['items'][album]['release_date']
        releases.append(date[:4])
    return releases

def analyse_release_history(artists):
    artist_nr = input("Artist Nr: ")
    if artist_nr in artists.keys():    
        try:
            id = artists[artist_nr][1]
            spotify_data = read_release_history(artist_nr)
            release_dates = get_release_dates_of_artist(spotify_data)
            plot_release_history(release_dates, artists[artist_nr][0])
        except: 
            print("An error occured. Check your internet connection.")
    else:
        print("Artist number not available. Try again.")

def get_explicit_charts(data) :
    explicity_counter = 0
    for song in data['tracks']['items'] :
        if song['track']['explicit'] == True :
            explicity_counter += 1
    return (explicity_counter / len(data['tracks']['items']))* 100

def get_explicit_top(data):
    explicity_counter = 0
    for song in data['tracks'] :
        if song['explicit'] == True :
            explicity_counter += 1
    return (explicity_counter / len(data['tracks']))* 100

def analyse_personal_lyrics(artist_dict, artist_nr) :
    artist_name = artist_dict[artist_nr][0]
    print("Let's analyze their lyrics!")
    track = input("Please input one of their songs: ")
    try:
        lyrics_response = retrieve_lyrics(artist_name, track)
    except: 
        print("Internet connection failed.") 
        return None
    if lyrics_response == {'error': 'No lyrics found'}:
        print("This song was not found.")
    else:    
        lyrics_data = lyrics_response['lyrics']

        me_list = re.findall(r"I|me|mine", lyrics_data)
        they_list = re.findall(r"he|him|she|her|they|them", lyrics_data)
        you_list = re.findall(r"you|your|yours", lyrics_data)

        if len(me_list) > len(you_list) and len(me_list) > len(they_list) : # me
            print(f"{track} by {artist_name} is a selfish song, all about 'me'!")
        elif len(you_list) > len(me_list) and len(you_list) > len(they_list) : # you
            print(f"{track} by {artist_name} is a song all about 'you'!")
        elif len(they_list) > len(me_list) and len(they_list) > len(you_list) : # they
            print(f"{track} by {artist_name} mentions other people a lot.")
        if len(me_list) == len(you_list) and len(me_list) > len(they_list) : # me you
            print(f"{track} by {artist_name} is equally about 'you' and 'me'. Lovely!")
        elif len(you_list) == len(they_list) and len(you_list) > len(me_list) : # you they
            print(f"{track} by {artist_name} is all about 'you' and other people.")
        elif len(they_list) == len(me_list) and len(they_list) > len(you_list) : # me they
            print(f"{track} by {artist_name} talks about the singer and people around them equally.")
        elif len(they_list) == len(me_list) == len(you_list) : # me you they
            print(f"{track} by {artist_name} is truly a song about everyone :)")

        buckets = [len(me_list), len(you_list), len(they_list)]
        plot_pronouns(buckets, track)

def analyze_wikipage_views(view_data):
    daily_views = []
    for day in view_data["items"]:
        daily_views.append(day["views"])
    total_views = sum(daily_views)
    avg_views = math.ceil(total_views / len(daily_views))
    return [total_views, avg_views]

def compare_duration_artists(artist_nr_1, artist_nr_2, artists_dict):
    top_data_1 = read_top(artist_nr_1)
    top_data_2 = read_top(artist_nr_2)
    artist_name_1 = artists_dict[artist_nr_1][0]
    artist_name_2 = artists_dict[artist_nr_2][0]
    top_data_1 = top_data_1['tracks']
    top_data_2 = top_data_2['tracks']
    duration_artist_1 = []
    duration_artist_2 = []
    for song in top_data_1:
        duration = song['duration_ms']
        duration_artist_1.append(duration)
    for song in top_data_2:
        duration = song['duration_ms']
        duration_artist_2.append(duration)

    total_duration_1 = 0
    for song in duration_artist_1:
        total_duration_1 += song/1000
    mean_duration_1 = int(total_duration_1 / 10)
    minutes_1 = mean_duration_1 // 60
    seconds_1 = mean_duration_1 % 60
    print(f"Average duration of Top 10 {artist_name_1} Songs: {minutes_1}:{seconds_1:02d} minutes")

    total_duration_2 = 0
    for song in duration_artist_2:
        total_duration_2 += song/1000
    mean_duration_2 = int(total_duration_2 / 10)
    minutes_2 = mean_duration_2 // 60
    seconds_2 = mean_duration_2 % 60
    print(f"Average duration of Top 10 {artist_name_2} Songs: {minutes_2}:{seconds_2:02d} minutes")

    if mean_duration_1 > mean_duration_2:
        difference = mean_duration_1 - mean_duration_2
        print(f"{artist_name_1}'s songs are on average {difference} seconds longer.")
    elif mean_duration_1 < mean_duration_2:
        difference = mean_duration_2 - mean_duration_1
        print(f"{artist_name_2}'s songs are on average {difference} seconds longer.")
    else: print(f"The songs of {artist_name_1} and {artist_name_2} have on average the same duration!")

def compare_duration_charts(artist_nr, artists_dict):
    data_artist = read_top(artist_nr)
    data_charts = read_charts()
    artist_name = artists_dict[artist_nr][0]
    duration_artist = []
    duration_charts = []
    for song in data_artist['tracks']:
        duration = song['duration_ms']
        duration_artist.append(duration)
    for song in data_charts['tracks']['items']:
        duration = song['track']['duration_ms']
        duration_charts.append(duration)

    total_duration_artist = 0
    for song in duration_artist:
        total_duration_artist += song/1000
    mean_duration_artist = int(total_duration_artist / 10)
    minutes_artist = mean_duration_artist // 60
    seconds_artist = mean_duration_artist % 60
    print(f"Average duration of Top 10 {artist_name} Songs: {minutes_artist}:{seconds_artist:02d} minutes")

    total_duration_charts = 0
    for song in duration_charts:
        total_duration_charts += song/1000
    mean_duration_charts = int(total_duration_charts / 50)
    minutes_charts = mean_duration_charts // 60
    seconds_charts = mean_duration_charts % 60
    print(f"Average duration of the charts: {minutes_charts}:{seconds_charts:02d} minutes")

    if mean_duration_artist > mean_duration_charts:
        difference = mean_duration_artist - mean_duration_charts
        print(f"{artist_name}'s songs are on average {difference} seconds longer")
    elif mean_duration_artist < mean_duration_charts:
        difference = mean_duration_charts - mean_duration_artist
        print(f"The charts' songs are on average {difference} seconds longer")
    else: print(f"The songs of {artist_name} and the charts have on average the same duration!")

def calculate_pop_index_artist(artist_nr, artist_name):
    top_tracks = read_top(artist_nr)
    id_list = []
    for song in top_tracks['tracks']:
        id_list.append(song['id'])
    audio_features = read_features(artist_nr)
    mean_popularity = 0
    mean_danceability = 0
    mean_duration = 0
    for song in top_tracks['tracks']:
        mean_popularity += (song['popularity'] / 100)
        mean_duration += (song['duration_ms'] /100000)
    mean_popularity /= 10
    mean_duration /= 10
    for song in audio_features['audio_features']:
        mean_danceability += song['danceability']
    mean_danceability /= 10
    view_data = retrieve_wikipage_views_month(artist_name)
    wikipedia_views = analyze_wikipage_views(view_data)
    wikipedia_views_month = wikipedia_views[0]
    pop_index = (mean_popularity * mean_danceability * (wikipedia_views_month **0.01) / (mean_duration)**0.5) * 2
    return pop_index

def calculate_pop_index_charts():
    charts = read_charts()
    audio_features = read_charts_features()
    mean_popularity = 0
    mean_danceability = 0
    mean_duration = 0
    mean__wiki_views = 0
    for song in charts['tracks']['items']:
        mean_popularity += (song['track']['popularity'] / 100)
        mean_duration += (song['track']['duration_ms'] /100000)
    mean_popularity /= 50
    mean_duration /= 50
    for song in audio_features['audio_features']:
        mean_danceability += song['danceability']
    mean_danceability /= 50
    wiki_counter = 0
    for song in charts['tracks']['items']:
        artist_name = song['track']['artists'][0]['name']
        try:
            view_data = retrieve_wikipage_views_month(artist_name, print_errors = False)
            wikipedia_views = analyze_wikipage_views(view_data)
            mean__wiki_views += wikipedia_views[0]
            wiki_counter += 1
        except: pass
    mean__wiki_views /= wiki_counter
    pop_index = (mean_popularity * mean_danceability * (mean__wiki_views **0.01)/ (mean_duration)**0.5) * 2
    return pop_index

def compare_to_artist(artists):
    print_artists(artists)
    artist_nr_1 = input("Artist Nr 1: ")
    artist_nr_2 = input("Artist Nr 2: ")
    if artist_nr_1 in artists.keys() and artist_nr_2 in artists.keys():
        artist_1_name = artists[artist_nr_1][0]
        artist_2_name = artists[artist_nr_2][0]
        top_artist_1 = read_top(artist_nr_1)
        top_artist_2 = read_top(artist_nr_2)
        expl_artist_1 = get_explicit_top(top_artist_1)
        expl_artist_2 = get_explicit_top(top_artist_2)
        pop_index_artist_1 = calculate_pop_index_artist(artist_nr_1, artist_1_name)
        pop_index_artist_2 = calculate_pop_index_artist(artist_nr_2, artist_2_name)

        print(f"The Top 10 songs of {artist_1_name} are {expl_artist_1}% explicit.")
        print(f"The Top 10 songs of {artist_2_name} are {expl_artist_2}% explicit.")
        if expl_artist_1 < expl_artist_2:
            print(f"{artist_1_name} is family friendly compared to {artist_2_name}.")
        elif expl_artist_1 == expl_artist_2 and expl_artist_1 <= 30.0:
            print(f"{artist_1_name} is just as peaceful as {artist_2_name}.")
        elif expl_artist_1 == expl_artist_2:
            print(f"{artist_1_name} is just as outrageous as {artist_2_name}.")
        else:
            print(f"{artist_2_name} is family friendly compared to {artist_1_name}.")

        print(f"{artist_1_name} has a POP-Index of {pop_index_artist_1:.2f}.")
        print(f"{artist_2_name} has a POP-Index of {pop_index_artist_2:.2f}.")
        if pop_index_artist_1 > pop_index_artist_2:
            print(f"{artist_1_name} is the real Pop Diva compared to {artist_2_name}.")
        elif pop_index_artist_1 == pop_index_artist_2 and pop_index_artist_1 <= 0.30:
            print(f"{artist_1_name} is just as boring as {artist_2_name}.")
        elif pop_index_artist_1 == pop_index_artist_2:
            print(f"{artist_1_name} is just as awesome as {artist_2_name}.")
        else:
            print(f"{artist_2_name} is the real Pop Diva compared to {artist_1_name}.")
        
        compare_duration_artists(artist_nr_1, artist_nr_2, artists)

    elif artist_nr_1 == artist_nr_2 :
        print("Choose two different artists.")    
    else: print("At least one of the artist numbers does not exist. Try again.")

def compare_to_charts(artists):
    print_artists(artists)
    artist_nr = input("Artist Nr: ")
    artist_name = artists[artist_nr][0]
    charts = read_charts()
    top_artist = read_top(artist_nr)
    expl_charts = get_explicit_charts(charts)
    expl_artist = get_explicit_top(top_artist)
    pop_index_artist = calculate_pop_index_artist(artist_nr, artist_name)
    pop_index_charts = calculate_pop_index_charts()
    
    print(f"This week's top hits playlist is {expl_charts}% explicit.")
    print(f"The Top 10 songs of {artist_name} are {expl_artist}% explicit.")
    if expl_charts <= expl_artist:
        print(f"{artist_name} is not afraid of swearing!")
    else:
        print(f"{artist_name} is family friendly (compared to the charts).")

    print(f"{artist_name} has a POP-Index of {pop_index_artist:.2f}.")
    print(f"The charts have a POP-Index of {pop_index_charts:.2f}.")
    if pop_index_artist > pop_index_charts:
        print(f"{artist_name} is the real Pop Diva compared to the charts!")
    elif pop_index_artist == pop_index_charts and pop_index_artist <= 0.30:
        print(f"{artist_name} is just as boring as the charts!")
    elif pop_index_artist == pop_index_charts:
        print(f"{artist_name} is just as awesome as the charts!")
    else:
        print(f"The charts are full of Pop Divas compared to {artist_name}.")
    compare_duration_charts(artist_nr, artists)

# Add artist function
def add_artist(name, artists): 
    try:
        id, actual_name = retrieve_artist_id(name)
        new_key = str(int(max(artists.keys()))+1)                  
        top_tracks = retrieve_top_tracks(new_key, id)
        retrieve_release_history(id, new_key)
        id_list = []
        for song in top_tracks['tracks']:
            id_list.append(song['id'])
        print("Wait 30 seconds - due to limited requests to spotfy API")
        time.sleep(30) 
        retrieve_audio_features(id_list, new_key)
        artists.update({new_key : [actual_name, id]})
        with open("resources/artists.json", 'w') as file:
            json.dump(artists, file)
        return artists
    except: 
        return None

def main() :
    artists = read_artists()
    if artists == None: 
        artists = {}
        print("No artists have been added yet.")
    option = ""
    while option != "exit" :
        print_query()
        option = input()
        match option :
            case "0": 
                print_artists(artists)
           
            case "1" : # artist's release history
                print_artists(artists)
                analyse_release_history(artists)

            case "2" : # how personal lyrics are 
                #Add wikipedia API here
                print_artists(artists)
                artist_nr = input("Artist Nr: ")
                print_wiki_info_text(artists[artist_nr][0])
                analyse_personal_lyrics(artists, artist_nr)

            case "3" : # Comparing artists  
                print("Do you want to compare an artist with another artist or with this week's charts?")
                choice = input("Type 'artist' to compare with artist, or 'charts' to compare with charts: \n").lower()
                if choice == "artist":
                    compare_to_artist(artists)
                elif choice == "charts":
                    compare_to_charts(artists)
                else: print("Invalid input.")    
           
            case "x": # Add artist
                name = input("Name of artist you want to add:")
                new_artists = add_artist(name, artists)
                if new_artists == None:
                    print("An error occured. Check your internet connection.")
                else: artists = new_artists

            case "exit" :                
                print("Thank you for using our tool! See you next time.")
            
            case _ :
                print("Invalid input, try again!")

if __name__ == "__main__" :
    main()