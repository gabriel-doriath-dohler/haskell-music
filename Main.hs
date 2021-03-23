module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List

type Pulse = Float
type Wave = [Pulse]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Volume = Float
type Octave = Float

data Note
    = Do
    | DoSharp
    | Re
    | ReSharp
    | Mi
    | Fa
    | FaSharp
    | Sol
    | SolSharp
    | La
    | LaSharp
    | Si
    deriving (Show, Eq)

type NoteParameters = (Octave, Volume)
type NoteWithHarmonics = [(Note, NoteParameters)]
type Chord = (Beats, [NoteWithHarmonics], Volume)
type Melody = [Chord]

-- Melodies
takeFive :: Melody
takeFive = [ (0.5, [[(Sol, (0, 1.0))]], 1.0)
           , (0.5, [[(Do, (1.0, 1.0))]], 1.0)
           , (0.5, [[(ReSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Fa, (1.0, 1.0))]], 1.0)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Sol, (1.0, 1.0))]], 1.0)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Fa, (1.0, 1.0))]], 1.0)
           , (1.0, [[(ReSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Sol, (0, 1.0))]], 1.0)
           , (0.25, [[(SolSharp, (0, 1.0))]], 1.0)
           , (0.25, [[(La, (0, 1.0))]], 1.0)
           , (1.0, [[(LaSharp, (0, 1.0))]], 1.0) -- 14
           , (3.0, [[(Do, (1.0, 1.0))]], 1.0)

           , (0.25, [[(Re, (1.0, 1.0))]], 1.0)
           , (0.25, [[(ReSharp, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Re, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Do, (1.0, 1.0))]], 1.0)
           , (1.0, [[(LaSharp, (0.0, 1.0))]], 1.0) -- 15
           , (3.0, [[(Do, (1.0, 1.0))]], 1.0)

           , (0.25, [[(LaSharp, (0.0, 1.0))]], 1.0)
           , (0.25, [[(Do, (1.0, 1.0))]], 1.0)
           , (0.25, [[(LaSharp, (0.0, 1.0))]], 1.0)
           , (0.25, [[(Sol, (0.0, 1.0))]], 1.0)
           , (1.0, [[(Fa, (0.0, 1.0))]], 1.0) -- 16
           , (3.0, [[(Sol, (0.0, 1.0))]], 1.0)


           , (0.5, [[(Sol, (0, 1.0))]], 1.0)
           , (0.5, [[(Do, (1.0, 1.0))]], 1.0)
           , (0.5, [[(ReSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Fa, (1.0, 1.0))]], 1.0)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Sol, (1.0, 1.0))]], 1.0)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Fa, (1.0, 1.0))]], 1.0)
           , (1.0, [[(ReSharp, (1.0, 1.0))]], 1.0)
           , (0.5, [[(Sol, (0, 1.0))]], 1.0)
           , (0.25, [[(SolSharp, (0, 1.0))]], 1.0)
           , (0.25, [[(La, (0, 1.0))]], 1.0)
           , (1.0, [[(LaSharp, (0, 1.0))]], 1.0) -- 18
           , (3.0, [[(Do, (1.0, 1.0))]], 1.0)

           , (0.25, [[(LaSharp, (0.0, 1.0))]], 1.0)
           , (0.25, [[(Do, (1.0, 1.0))]], 1.0)
           , (0.25, [[(LaSharp, (0.0, 1.0))]], 1.0)
           , (0.25, [[(Sol, (0.0, 1.0))]], 1.0)
           , (1.0, [[(Fa, (0.0, 1.0))]], 1.0) -- 19
           , (3.0, [[(Sol, (0.0, 1.0))]], 1.0)

           , (0.25, [[(Re, (1.0, 1.0))]], 1.0)
           , (0.25, [[(ReSharp, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Re, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Do, (1.0, 1.0))]], 1.0)
           , (1.0, [[(LaSharp, (0.0, 1.0))]], 1.0) -- 20
           , (3.0, [[(Do, (1.0, 1.0))]], 1.0)

           , (2.0, [[(Do, (1.0, 0.0))]], 0.0)


           , (0.5, [[(Do, (2.0, 1.0))]], 0.7) -- 21
           , (0.75, [[(ReSharp, (2.0, 1.0))]], 1.0)
           , (0.25, [[(ReSharp, (2.0, 0.0))]], 0.0)
           , (0.5, [[(Do, (2.0, 1.0))]], 0.7)
           , (0.75, [[(SolSharp, (1.0, 1.0))]], 1.0)
           , (0.25, [[(SolSharp, (1.0, 0.0))]], 0.0)
           , (0.5, [[(Fa, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Sol, (1.0, 1.0))]], 0.7)
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(La, (1.0, 1.0))]], 0.7)

           -- 22
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Re, (2.0, 1.0))]], 1.0)
           , (0.25, [[(Re, (2.0, 0.0))]], 0.0)
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Sol, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Sol, (1.0, 0.0))]], 0.0)
           , (0.5, [[(ReSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Fa, (1.0, 1.0))]], 0.7)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Sol, (1.0, 1.0))]], 0.7)

           -- 23
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Do, (2.0, 1.0))]], 1.0)
           , (0.25, [[(Do, (2.0, 0.0))]], 0.0)
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Fa, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Fa, (1.0, 0.0))]], 0.0)
           , (0.5, [[(Re, (1.0, 1.0))]], 0.7)
           , (0.5, [[(ReSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Fa, (1.0, 1.0))]], 0.7)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 0.7)

           -- 24
           , (0.5, [[(Sol, (1.0, 1.0))]], 0.7)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Sol, (1.0, 1.0))]], 0.7)
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (1.0, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(La, (1.0, 1.0))]], 0.7)
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Si, (1.0, 1.0))]], 0.7)

           -- 25
           , (2.0, [[(Do, (1.0, 0.0))]], 0.0)
           , (0.5, [[(Do, (2.0, 1.0))]], 0.7)
           , (0.75, [[(ReSharp, (2.0, 1.0))]], 1.0)
           , (0.25, [[(ReSharp, (2.0, 0.0))]], 0.0)
           , (0.5, [[(Do, (2.0, 1.0))]], 0.7)
           , (0.75, [[(SolSharp, (1.0, 1.0))]], 1.0)
           , (0.25, [[(SolSharp, (1.0, 0.0))]], 0.0)
           , (0.5, [[(Fa, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Sol, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Sol, (1.0, 0.0))]], 0.0)
           , (0.5, [[(La, (1.0, 1.0))]], 0.7)

           -- 26
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Re, (2.0, 1.0))]], 1.0)
           , (0.25, [[(Re, (2.0, 0.0))]], 0.0)
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Sol, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Sol, (1.0, 0.0))]], 0.0)
           , (0.5, [[(ReSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Fa, (1.0, 1.0))]], 0.7)
           , (0.5, [[(FaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Sol, (1.0, 1.0))]], 0.7)

           -- 27
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Do, (2.0, 1.0))]], 1.0)
           , (0.25, [[(Do, (2.0, 0.0))]], 0.0)
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (0.75, [[(Fa, (1.0, 1.0))]], 1.0)
           , (0.25, [[(Fa, (1.0, 0.0))]], 0.0)
           , (0.5, [[(Re, (1.0, 1.0))]], 0.7)
           , (0.5, [[(Fa, (1.0, 1.0))]], 0.7)
           , (0.5, [[(LaSharp, (1.0, 1.0))]], 0.7)
           , (0.5, [[(SolSharp, (1.0, 1.0))]], 0.7)
           , (3.0, [[(Sol, (1.0, 1.0))]], 0.7) -- 28
           ]

-- Parameters

outputFilePath :: FilePath
outputFilePath = "output.bin"

masterVolume :: Float
masterVolume = 0.7

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- Generate a wave

melody :: Melody -> Wave
melody = concat . map chord

chord :: Chord -> Wave
chord (beats, notes, vol) =
    map (* vol) . superpose $ map (noteWithHarmonics beats) notes

noteWithHarmonics :: Beats -> NoteWithHarmonics -> Wave
noteWithHarmonics beats = superpose . map (note beats)

note :: Beats -> (Note, NoteParameters) -> Wave
note beats (noteName, (nb, vol)) =
       map (* vol) $ freq frequency (beats * beatDuration)
    where
        n = noteNumber noteName + 12.0 * nb
        frequency = f n

freq :: Hz -> Seconds -> Wave
freq hz duration =
    map (* masterVolume) $ zipWith3 (\x y z -> x * y * z) release attack output
    where
        step = (hz * 2 * pi) / sampleRate

        attack :: Wave
        attack = map (min 1.0) [0.0,0.001 ..]

        release :: Wave
        release = reverse $ take (length output) attack

        output :: Wave
        output = map sin $ map (* step) [0.0 .. sampleRate * duration]

noteNumber :: Note -> Float
noteNumber La       = 0.0
noteNumber LaSharp  = 1.0
noteNumber Si       = 2.0
noteNumber Do       = -9.0
noteNumber DoSharp  = -8.0
noteNumber Re       = -7.0
noteNumber ReSharp  = -6.0
noteNumber Mi       = -5.0
noteNumber Fa       = -4.0
noteNumber FaSharp  = -3.0
noteNumber Sol      = -2.0
noteNumber SolSharp = -1.0

superpose :: [Wave] -> Wave
superpose = map sum . transpose

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

-- Save and play

save :: FilePath -> Wave -> IO ()
save filePath wave = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave

play :: Wave -> IO ()
play wave = do
    save outputFilePath wave
    _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()

main :: IO ()
main = save outputFilePath (melody takeFive)

