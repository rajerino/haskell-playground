{-----------------------------------------------------------------------------
    --forked tomato-rubato-openal
    
    SoundTest -- Modifying included example program to function as live coding
                 of sound from ghci prompt.
                 Commands:
                 sineMod      f,s,a --plays enveloped sin wave of frequency f
                                       with amplitude a
                 playNote     f,s,a --plays note of frequency f for s seconds 
                                       with amplitude a
                 
    References:
                 Bryn Keller (xoltar@xoltar.org) 
                   <http://www.xoltar.org/2003/sep/09/haskellLoops.html>
                 HaskellWiki
                   <http://www.haskell.org/haskellwiki/>
                   
------------------------------------------------------------------------------}
import Sound.Tomato.Speakers
import Data.IORef


--import Control.Concurrent.Timer

-- play simple envelope modulated sine wave of given frequency, time, amplitude
sineMod :: (Float, Float,Float) -> IO ()
sineMod (freq, sec, amp) = fromPure sec amp $
    \t -> 0.4 * sine (freq*t) * sine (15*t) * exp (negate $ t/a)
    where
    sine t = sin (2*pi*t)
    a      = 0.5

-- play unmodulated note of given frequency, time, amplitude
playNote :: (Float,Float,Float) -> IO ()
playNote (freq, sec ,amp) = fromPure sec amp $
    \t -> sine (freq*t)
    where
    sine t = sin (2*pi*t)

-- play a list of notes using playNote
playSong :: [(Float,Float,Float)] -> IO ()
playSong song = if not (null (tail song)) 
                      then do
                        (playNote (head song)) 
                        (playSong (tail song))
                      else do
                        (playNote (head song)) 
                        putStr "Fin"

-- play a list of notes on a loop
playLoop :: [(Float,Float,Float)] -> IO ()
playLoop song = ploop song song

ploop :: [(Float,Float,Float)] -> [(Float,Float,Float)] -> IO ()
ploop song song_ = if not (null (tail song_)) 
                      then do
                        (playNote (head song_)) 
                        (ploop song (tail song_))
                      else do
                        (playNote (head song_))
                        (ploop song song)
                        


--parseNote :: [Char] -> (Float,Float,Float)
--parseNote note = (freq, sec, amp) 
--     where
--     freq = getNote note
--     sec  = (read (getNoteDuration note):: Float)
--     amp  = (read (getNoteVelocity note) :: Float)


getNote :: [Char] -> Float  --uses noteList and noteFreqList to get frequency
getNote (note : octave) = getFreq note (read octave :: Float)

getNoteDuration :: String -> String --note duration in seconds
getNoteDuration note = (tail (tail (tail note)))

getNoteVelocity :: String -> String --note velocity, aka amplitude of note function
getNoteVelocity note = (head (tail (tail ((read note) :: [String]))))

getFreq :: Char -> Float -> Float
getFreq note octave = (1 + octave) * (noteFreqList !! (getFreqListIndex note))

noteList     = ["a","b","c","d","e","f","g","A","C","D","F","G"]    --sharps are denoted with capitalization
noteFreqList = [27.50,30.87,16.35,18.35,20.60,21.83,24.50,29.14,17.32,19.45,23.12,25.96]
--use frequency ranges instead of this dictionary to make smooth, pitch-bendable octaves 

cMajorScale = [(523.25 :: Float,0.2 :: Float,1 :: Float),(587.33 :: Float,0.2 :: Float,1 :: Float),(659.26 :: Float,0.2 :: Float,1 :: Float),(698.46 :: Float,0.2 :: Float,1 :: Float),(783.99 :: Float,0.2 :: Float,1 :: Float),(880 :: Float,0.2 :: Float,1 :: Float),(987.77 :: Float,0.2 :: Float,1 :: Float),(1046.5 :: Float,0.2 :: Float,1 :: Float)]

amzGr = [(392 :: Float,0.25 :: Float,1 :: Float),(523.25 :: Float,0.5 :: Float,1 :: Float),(659.26 :: Float,0.125 :: Float,1 :: Float),(523.25 :: Float,0.125 :: Float,1),(659.26 :: Float,0.5 :: Float,1 :: Float),(587.33 :: Float,0.25 :: Float,1 :: Float),(523.25 :: Float,0.5 :: Float,1 :: Float),(440 :: Float,0.25 :: Float,1 :: Float),(392 :: Float,0.5 :: Float,1 :: Float),(392 :: Float,0.25 :: Float,1 :: Float),(523.25 :: Float,0.5 :: Float,1 :: Float),(659.26 :: Float,0.125 :: Float,1 :: Float),(523.25 :: Float,0.125 :: Float,1 :: Float),(659.26 :: Float,0.5 :: Float,1 :: Float),(587.33 :: Float,0.125 :: Float,1 :: Float),(659.26 :: Float,0.125 :: Float,1 :: Float),(783.99 :: Float,0.5 :: Float,1 :: Float)]

getFreqListIndex :: Char -> Int
getFreqListIndex noteName = head [x | x <- [0..11], (noteList !! x) == [noteName]]
{--
getNoteName :: String -> String
getNoteName note = head note : head (tail note) : []

--}
type Time = Float -- in seconds

-- Play wave form given by a pure function
fromPure :: Float -> Float -> (Time -> Sample) -> IO ()
fromPure sec amp f = withSpeakers sampleRate 512 $ \s -> playSamples s sound
    where
    sampleRate  = 22050
    dt          = 1 / sampleRate -- time in seconds of a single sample
    sound       = [amp * f (dt*t) | t <- [0..(sampleRate*sec)]]