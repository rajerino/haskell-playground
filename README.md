## rajerino's Haskell playground

Where I'm learning to use Haskell.

### SoundTest.hs

SoundTest uses the example from the tomato-rubato library to develop some live coding tools. So far it is just a few basic functions:
* **playNote(Float,Float,Float)**

	playNote plays the note specified by the tuple (note-frequency, duration-in-seconds, note-amplitude) 

* **playSong([(Float,Float,Float)])**

	playSong plays the notes specified in an ordered list of note tuples

* **playLoop([(Float,Float,Float)])**

	playLoop plays on an infinite loop the notes specified in an ordered list of note tuples

* **getNote([Char])**

	getNote returns freqency of a string in the format of "%s%d", where %s is the note letter and %d is the desired octave
