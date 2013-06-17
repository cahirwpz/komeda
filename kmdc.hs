{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-unused-do-bind #-}

-- import qualified Data.ByteString.Lazy as B
-- import Control.Monad.Reader
import Data.Char (toUpper)
-- import Data.WAVE
-- import Data.Word
-- import Data.Bits
-- import Data.Binary
import Data.Ratio
import GHC.Float (double2Float)
import qualified Data.String.Utils as Str
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Perm

-- abstract tree representation of parsed music file

type Nat = Int
type Frac = Ratio Nat

data Semitone = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
data NoteLength = NoteLength Frac
data NotePitch = NotePitch { semitone :: Semitone, octave :: Nat }
data Note = Note NotePitch NoteLength
data Playable = Play Note | PlayTie [Note] | Rest NoteLength

data Setting = SetTempo Nat          -- beats per minute (in quarter-notes)
             | ModTempo Int          -- modify tempo by bpm
             | SetUnit Nat           -- default note length (1/n note)
             | ModPitch Int          -- pitch change by n halftones
             | SetVolume Float       -- set volume (default value: 1.0)
             | ModVolume Float       -- modify volume by f
             | UseVoice String       -- index in instrument list

data Command = Sequence [Playable]       -- sequence of playable commands
             | Modify [Setting]          -- setting change
             | Insert String             -- play pattern of given name
             | Times Nat [Command]       -- repeat command stream n times
             | Forever [Command]         -- repeat command stream forever
             | On Nat [Command]          -- on i-th iteration execute commands
             | With [Setting] [Command]  -- change setting for given scope

data Voice = Voice { audioFile    :: String,
                     defaultPitch :: NotePitch }
  deriving Show

data Music = Music { voices   :: [(String, Voice)],
                     patterns :: [(String, [Command])],
                     channels :: [(Int, [Command])] }
   deriving Show

-- pretty printers for AST types

show_list l = "[" ++ Str.join ", " (map show l) ++ "]"
show_seq l  = Str.join " " (map show l)
show_diff n = if n >= 0 then "+" ++ show n else show n

instance Show Semitone where
  show C  = "c"
  show Cs = "c#"
  show D  = "d"
  show Ds = "d#"
  show E  = "e"
  show F  = "f"
  show Fs = "f#"
  show G  = "g"
  show Gs = "g#"
  show A  = "a"
  show As = "a#"
  show B  = "b"

instance Show NoteLength where
  show (NoteLength l) =
    let n = numerator l
        d = denominator l
        in case (n, d) of
                (1, 1) -> ""
                (1, _) -> "/" ++ show d
                (_, 1) -> show n
                _      -> show n ++ "/" ++ show d

instance Show NotePitch where
  show (NotePitch semitone' octave') =
    if octave' <= 3
      then show semitone' ++ take (3 - octave') (repeat ',')
      else map toUpper (show semitone') ++ take (octave' - 4) (repeat '\'')

instance Show Note where
  show (Note pitch' length') = show pitch' ++ show length'

instance Show Playable where
  show (Play n)     = show n
  show (Rest l)     = "R" ++ show l
  show (PlayTie nl) = "(" ++ show_seq nl ++ ")"

instance Show Setting where
  show (SetTempo n)  = "tempo " ++ show n
  show (ModTempo n)  = "tempo " ++ show_diff n
  show (SetUnit n)   = "unit 1/" ++ show n
  show (ModPitch n)  = "pitch " ++ show_diff n
  show (SetVolume f) = "volume " ++ show f
  show (ModVolume f) = "volume " ++ show_diff f
  show (UseVoice s)  = "voice " ++ show s

instance Show Command where
  show (Sequence l) = show_seq l
  show (Modify l)   = show_list l
  show (Insert s)   = "{insert " ++ show s ++ "}"
  show (Times n l)  = "{times " ++ show n ++ ": " ++ show_seq l ++ "}"
  show (Forever l)  = "{forever: " ++ show_seq l ++ "}"
  show (On n l)     = "{on " ++ show n ++ ": " ++ show_seq l ++ "}"
  show (With sl cl) = "{with " ++ show_list sl ++ ": " ++ show_seq cl ++ "}"

-- let's reuse some of Java parser elements and define some parser helpers

yamaStyle = javaStyle

lexer = Token.makeTokenParser yamaStyle
lexeme = Token.lexeme lexer
braces = Token.braces lexer
parens = Token.parens lexer
brackets = Token.brackets lexer
natural = Token.natural lexer
colon = Token.colon lexer
float = Token.float lexer
symbol = Token.symbol lexer
commaSep1 = Token.commaSep1 lexer
stringLiteral = Token.stringLiteral lexer
whiteSpace = Token.whiteSpace lexer

sign = oneOf "+-"
tryParser p = option Nothing $ do {x <- p; return $ Just x}

-- Playable parsers

readDenominator = do
  char '/'
  den <- natural
  return den

readNoteLength = do
  num <- option 1 natural
  den <- option 1 readDenominator
  return $ NoteLength (fromIntegral num % fromIntegral den)

readFlatLowSemitone = do
  t <- oneOf "eb"
  return $ case t of
                'e' -> E
                'b' -> B

readRichLowSemitone = do
  t <- oneOf "cdfga"
  s <- tryParser (char '#')
  let t' = case s of Just '#' -> [t, '#']; _ -> [t]
      in return $ case t' of
                       "c"  -> C
                       "c#" -> Cs
                       "d"  -> D
                       "d#" -> Ds
                       "f"  -> F
                       "f#" -> Fs
                       "g"  -> G
                       "g#" -> Gs
                       "a"  -> A
                       "a#" -> As

readFlatHighSemitone = do
  t <- oneOf "EB"
  return $ case t of
                'E' -> E
                'B' -> B

readRichHighSemitone = do
  t <- oneOf "CDFGA"
  s <- tryParser (char '#')
  let t' = case s of Just '#' -> [t, '#']; _ -> [t]
      in return $ case t' of
                       "C"  -> C
                       "C#" -> Cs
                       "D"  -> D
                       "D#" -> Ds
                       "F"  -> F
                       "F#" -> Fs
                       "G"  -> G
                       "G#" -> Gs
                       "A"  -> A
                       "A#" -> As

readLowNote = do
  t <- readFlatLowSemitone <|> readRichLowSemitone
  o <- many (char ',')
  return $ NotePitch t (3 - length o)

readHighNote = do
  t <- readFlatHighSemitone <|> readRichHighSemitone
  o <- many (char '\'')
  return $ NotePitch t (4 + length o)

readNotePitch = readLowNote <|> readHighNote

readNote = do
  p <- readNotePitch
  l <- readNoteLength
  return $ Note p l

readRest = do
  char 'R'
  l <- readNoteLength
  return $ Rest l

readTie = do
  ns <- parens $ many1 (lexeme readNote)
  return $ PlayTie ns

readPlayable = readTie <|> readRest <|> do {n <- readNote; return $ Play n}

-- Setting parsers

opt_sign = option Nothing (do { s <- sign; return (Just s); })

readTempo = do
  modifier <- opt_sign
  beats <- natural
  let bpm = fromIntegral beats
      in return $ case modifier of
                       Just '+' -> ModTempo bpm
                       Just '-' -> ModTempo (-bpm)
                       Nothing  -> SetTempo bpm

readVolume = do
  modifier <- opt_sign
  volume' <- float
  let vol = double2Float volume'
      in return $ case modifier of
                       Just '+' -> ModVolume vol
                       Just '-' -> ModVolume (-vol)
                       Nothing  -> SetVolume vol

readPitch = do
  modifier <- sign 
  semitones <- natural
  let n = fromIntegral semitones
      in return $ case modifier of
                       '+' -> ModPitch n
                       '-' -> ModPitch (-n)

readUnit = do
  char '1'
  char '/'
  unit <- natural
  return $ SetUnit (fromIntegral unit)

readVoice = do
  name <- stringLiteral
  return $ UseVoice name

readSetting = do
  name <- symbol "tempo" <|> (try $ symbol "volume") <|> symbol "pitch" <|> symbol "unit" <|> symbol "voice"
  case name of
       "tempo"  -> readTempo
       "volume" -> readVolume
       "pitch"  -> readPitch
       "unit"   -> readUnit
       "voice"  -> readVoice

-- Command parsers

readSettingsChange = brackets $ do
  settings <- commaSep1 readSetting
  return $ Modify settings

readSequence = do
  sequence <- many1 (lexeme readPlayable)
  return $ Sequence sequence

readTimes = do
  n <- natural
  colon
  cs <- commands
  return $ Times (fromIntegral n) cs

readInsert = do
  pattern <- stringLiteral
  return $ Insert pattern

readForever = do
  colon
  cs <- commands
  return $ Forever cs

readOn = do
  i <- natural
  colon
  cs <- commands
  return $ On (fromIntegral i) cs

readWith = do
  settings <- brackets $ commaSep1 readSetting
  colon
  cs <- commands
  return $ With settings cs

readControlCommand = braces $ do
  name <- symbol "times" <|> symbol "insert" <|> symbol "forever"
          <|> symbol "on" <|> symbol "with"
  case name of
            "times"   -> readTimes
            "insert"  -> readInsert
            "forever" -> readForever
            "on"      -> readOn
            "with"    -> readWith

command  = readSettingsChange <|> readSequence <|> readControlCommand
commands = many1 (lexeme command)

-- Voice parsers

readVoiceFile = do
  try $ symbol "file"
  lexeme stringLiteral

readVoicePitch = do
  try $ symbol "pitch"
  lexeme readNotePitch

readVoiceBody =
  permute $ Voice <$$> readVoiceFile <|?> (NotePitch C 4, readVoicePitch)

-- Top-level parsers

readVoiceDef = do
  symbol "voice"
  name <- stringLiteral
  body <- braces readVoiceBody
  return (name, body)

readPatternDef = do
  symbol "pattern"
  name <- stringLiteral
  cs <- braces commands
  return (name, cs)

readChannelDef = do
  symbol "channel"
  n <- natural
  cs <- braces commands
  return (fromIntegral n, cs)

music = do
  whiteSpace
  vs <- many1 (lexeme readVoiceDef)
  ps <- many (lexeme readPatternDef)
  cs <- many1 (lexeme readChannelDef)
  return $ Music vs ps cs

-- compiler part
{-
{-
  Binary representation of a command is composed of command byte and
  arbitrary number of following bytes that express arguments.

  The machine has a stack and following registers:
    r0 => instrument number (0 - 9)
    r1 => octave number (0 - 7)
    r2 => beats per minute (0 - 255)
    r3 => pitch
    r4 => volume
    r5 => branch pointer
    r6 => counter

  In command listing below each character represents single hexadecimal digit:
    x0 nn => (LOAD rx, nn) set register x with value of nn
    x1    => (PUSH x) push value of register x onto stack
    x2    => (POP x) set register x from value on stack
    x3 nn => sound output control
             x = 0 => (PLAY nn)
             x = 1 => (WAIT nn)
    x4    => branch control
             x = 0 => { r6 := r6 - 1; r6 > 0 => jmp r5 }
             x = 1 => { br := pc }
    ff    => halt (last instruction)
-}

data Register = InstR | OctR | BpmR | PitchR | VolR | BrR | CtrR
     deriving (Show, Enum)

data Instruction = LoadI Word8 Register
                 | PushI Register
                 | PopI Register
                 | LoadBr
                 | DecCtrBr
                 | PlayI Word8 
                 | WaitI Word8
                 | Halt
     deriving Show

opcode :: Word8 -> Word8 -> Word8
opcode a b = (a `shiftL` 4) .|. b

regnum :: Register -> Word8
regnum = fromIntegral . fromEnum

instance Binary Instruction where
  put (LoadI n r) = put (opcode (regnum r) 0) >> put n
  put (PushI r)   = put (opcode (regnum r) 1)
  put (PopI r)    = put (opcode (regnum r) 2)
  put (PlayI n)   = put (opcode 0 3) >> put n
  put (WaitI n)   = put (opcode 1 3) >> put n
  put LoadBr      = put (opcode 0 4)
  put DecCtrBr    = put (opcode 1 4)
  put Halt        = putWord8 255

type Environment = [(String, Instrument)]

compileChannels m =
  map (compileChannel (instruments m) . snd) (channels m)

compileChannel :: Environment -> [Command] -> [Instruction]
compileChannel ctx cs = concatMap translate cs
  where translate c = runReader (toInsn c) ctx

toInsn :: Command -> Reader Environment ([Instruction])
toInsn (BPM n) =
  return [LoadI (fromIntegral n) BpmR]
toInsn (Octave n) =
  return [LoadI (fromIntegral n) OctR]
toInsn (Select s) = do 
  n' <- asks (lookupIndex s)
  case n' of
       Just n -> return [LoadI n InstR]
       Nothing -> error $ "No such instrument: '" ++ s ++ "'"
toInsn (Loop n cs) = do
  ctx <- ask
  return ([PushI CtrR, PushI BrR, LoadI (fromInteger n) CtrR, LoadBr]
          ++ compileChannel ctx cs ++ [DecCtrBr, PopI BrR, PopI CtrR])
toInsn (Pitch (Just _) _) =
  return [LoadI 0 PitchR]
toInsn (Pitch Nothing _) =
  return [LoadI 0 PitchR]
toInsn (Wait n) =
  return [WaitI (fromIntegral n)]
toInsn (Play n) =
  return [PlayI (fromIntegral n)]

lookupIndex e l = findIndex' e l 0
  where findIndex' e (x:xs) i =
          if fst x == e
            then Just i
            else findIndex' e xs (i + 1)
        findIndex' e [] i = Nothing

-- binary representation

newtype Stream a = Stream { unstream :: [a] }
        deriving Show

instance Binary a => Binary (Stream a) where
  put (Stream l) =
    let toWord16 n = (fromIntegral n) :: Word16
        bytesTaken = foldl (+) 0 (map (B.length . encode) l)
        commands = length l
        in do
          put $ toWord16 bytesTaken
          put $ toWord16 commands
          mapM_ put l

data Sound = Sound { soundFrames    :: Word32,
                     soundFrameRate :: Word16,
                     soundSamples   :: [Int8] }
     deriving Show

instance Binary Sound where
  put sound = do
    put (soundFrames sound)
    put (soundFrameRate sound)
    mapM_ put (soundSamples sound)

data MusicRaw = MusicRaw { tracks :: [Stream Instruction],
                           sounds :: [Sound] }
     deriving Show

instance Binary MusicRaw where
  put music =
    let toWord16 n = (fromIntegral n) :: Word16
        mTracks = tracks music
        mSounds = sounds music
        in do
          put $ (toWord16 . length) mTracks
          mapM_ put mTracks
          put $ (toWord16 . length) mSounds
          mapM_ put mSounds

extractSound :: WAVE -> Sound
extractSound wave = Sound frames frameRate (map extract samples)
  where header = waveHeader wave
        samples = map head (waveSamples wave)
        frameRate = fromIntegral $ waveFrameRate header
        frames = case waveFrames header of
                      Just n -> fromIntegral n
                      Nothing -> error "Unknown frame number!"
        extract s = fromIntegral $ s `shiftR` 24 - 128

readSoundFiles :: Music -> IO [Sound]
readSoundFiles m = do
  samples <- mapM (getWAVEFile . samplePath . snd) (instruments m)
  return $ map extractSound samples

assemble m = do
  ss <- readSoundFiles m
  return $ MusicRaw (map Stream $ compileChannels m) ss
-}
-- main part

main = do
  c <- getContents
  case parse music "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right m -> do print m
                  {- raw <- assemble m
                  print raw
                  encodeFile "test.bin" raw -}
