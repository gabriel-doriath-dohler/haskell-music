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

data NoteName
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

data Note = Note
    { getName :: NoteName
    , getOctave :: Octave
    , getBeats :: Beats
    , getVolume :: Volume
    }

instance Show Note where
    show (Note name octave beats volume) =
        printf "%f,%f" frequency (beats * beatDuration)
        where
            n = noteNumber name + 12.0 * octave
            frequency = f n

type Melody = [Note]

type MelodyWithTime = [(Note, Float)]

showMelody m = unlines $ map show $ zip m $ scanl
    (\t -> (\note -> t + beatDuration * getBeats note)) 0 m

-- Melodies.
dosi :: Melody
dosi =
    [ Note Do 0 0.5 1
    , Note Do 0 0.5 1
    , Note Do 1 0.5 1
    , Note Do 0 0.5 1
    , Note Do 0 0.5 1
    , Note Si 0 0.5 1
    , Note Do 0 0.5 1
    , Note Do 0 0.5 1
    ]

lafasolla :: Melody
lafasolla =
    [ Note La 0 0.5 1
    , Note Do 0 0.5 1
    , Note Do 0 0.5 1
    , Note FaSharp 0 0.5 1
    , Note Do 0 0.5 1
    , Note Do 0 0.5 1
    , Note Sol 0 0.5 1
    , Note La 0 0.5 1
    ]

lafa :: Melody
lafa =
    [ Note La 0 0.5 1
    , Note Do 0 0.5 1
    , Note Do 0 0.5 1
    , Note FaSharp 0 2.5 1
    ]

fami :: Melody
fami =
    [ Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 1 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Mi 1 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    ]

theme3 :: Melody
theme3 = dosi ++ lafasolla ++ dosi

theme4 :: Melody
theme4 = theme3 ++ lafa

themeFa3 :: Melody
themeFa3 = fami ++
    [ Note Re 1 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Do 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Do 1 0.5 1
    , Note Re 1 0.5 1
    ] ++ fami

themeFa4 :: Melody
themeFa4 = themeFa3 ++
    [ Note Re 1 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Do 0 2.5 1
    ]

themeLa4 :: Melody
themeLa4 =
    [ Note LaSharp 0 0.5 1
    , Note LaSharp 0 0.5 1
    , Note LaSharp 1 0.5 1
    , Note LaSharp 0 0.5 1
    , Note LaSharp 0 0.5 1
    , Note Sol 1 0.5 1
    , Note LaSharp 0 0.5 1
    , Note LaSharp 0 0.5 1

    , Note Fa 1 0.5 1
    , Note LaSharp 0 0.5 1
    , Note LaSharp 0 0.5 1
    , Note Mi 1 0.5 1
    , Note LaSharp 0 0.5 1
    , Note LaSharp 0 0.5 1
    , Note Fa 1 0.5 1
    , Note Fa 1 0.5 1

    , Note Sol 0 0.5 1
    , Note Sol 0 0.5 1
    , Note Sol 1 0.5 1
    , Note Sol 0 0.5 1
    , Note Sol 0 0.5 1
    , Note Fa 1 0.5 1
    , Note Sol 0 0.5 1
    , Note Sol 0 0.5 1

    , Note Mi 1 0.5 1
    , Note Fa 0 0.5 1
    , Note Fa 0 0.5 1
    , Note Re 1 2.5 1
    ]

triolets1 :: Melody
triolets1 =
    [ Note ReSharp 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Sol 0 (1/3) 1
    ]

triolets2 :: Melody
triolets2 =
    [ Note Sol 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Do 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Mi 2 (1/3) 1
    , Note Sol 2 (1/3) 1
    ]

triolets3 :: Melody
triolets3 =
    [ Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note La 1 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    ]

triolets4 :: Melody
triolets4 =
    [ Note Sol 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Do 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Mi 2 (1/3) 1
    ]

triolets5 :: Melody
triolets5 =
    [ Note ReSharp 1 (1/3) 1
    , Note Do 1 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Sol 0 (1/3) 1
    ]

triolets6 :: Melody
triolets6 =
    [ Note Do 2 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Mi 2 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Si 1 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Mi 2 (1/3) 1
    , Note Do 2 (1/3) 1
    , Note Mi 1 (1/3) 1
    ]

triolets7 :: Melody
triolets7 =
    [ Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    , Note La 1 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Fa 1 (1/3) 1
    , Note Re 1 (1/3) 1
    , Note Si 0 (1/3) 1
    ]

triolets8 :: Melody
triolets8 =
    [ Note Sol 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Do 1 (1/3) 1
    , Note Sol 0 (1/3) 1
    , Note Do 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Si 2 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Sol 1 (1/3) 1
    , Note Mi 1 (1/3) 1
    , Note Do 1 (1/3) 1
    ]

repeatW :: Int -> [a] -> [a]
repeatW n = take n . cycle

doom :: Melody
doom = map (translate (-5 - 12)) $
    repeatW 3 theme4 ++
    theme3 ++ triolets1 ++
    theme4 ++
    theme3 ++ triolets2 ++
    themeFa4 ++
    themeFa3 ++ triolets3 ++
    repeatW 2 theme4 ++
    themeLa4 ++
    theme3 ++ triolets4 ++
    theme4 ++
    theme3 ++ triolets5 ++
    theme4 ++
    theme3 ++ triolets6 ++
    themeFa4 ++
    themeFa3 ++ triolets7 ++
    repeatW 2 theme4 ++
    themeLa4 ++
    theme3 ++ triolets8
    where
        translate n (Note name octave beats volume) =
            Note name (octave + n / 12) beats volume -- Ugly trick.

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
bpm = 255.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- Generate a wave

doomWave :: Wave
doomWave = waveFromMelody doom

waveFromMelody :: Melody -> Wave
waveFromMelody = concat . map waveFromNote

waveFromNote :: Note -> Wave
waveFromNote (Note name octave beats volume) =
       map (* volume) $ freq frequency (beats * beatDuration)
    where
        n = noteNumber name + 12.0 * octave
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

noteNumber :: NoteName -> Float
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

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

-- Save and play

save :: FilePath -> Wave -> IO ()
save filePath wave = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave

playDoom :: IO ()
playDoom = play doomWave

play :: Wave -> IO ()
play wave = do
    save outputFilePath wave
    _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()

main :: IO ()
main = putStrLn $ showMelody doom

