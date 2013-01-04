{-# OPTIONS_GHC -XFlexibleInstances -fno-warn-missing-methods #-}

import qualified Data.ByteString.Lazy as B
import Control.Monad.Reader
import Data.WAVE
import Data.Word
import Data.Bits
import Data.Binary
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language (javaStyle)

data Instrument = Sample String
     deriving Show

data Command = BPM Int                   -- bit = quarter-notes
             | Octave Int                -- preselects octave
             | Select String             -- index in instrument list
             | Loop Integer [Command]    -- how many times to repeat
             | Pitch (Maybe Int) String  -- [octave] pitch
             | Wait Int                  -- in quarter-notes
             | Play Int                  -- same as above
     deriving Show

data Music = Music { instruments :: [(String, Instrument)],
                     channels    :: [(Int, [Command])] }
     deriving Show

musicStyle = javaStyle

lexer = Token.makeTokenParser musicStyle
lexeme = Token.lexeme lexer
braces = Token.braces lexer
natural = Token.natural lexer
identifier = Token.identifier lexer
symbol = Token.symbol lexer
stringLiteral = Token.stringLiteral lexer
whiteSpace = Token.whiteSpace lexer

instrument = do
  symbol "Load"
  filename <- stringLiteral
  return $ Sample filename

namedInstrument = do
  symbol "Instrument"
  name <- stringLiteral
  inst <- braces instrument
  return (name, inst)

cmdBPM = do
  beats <- natural
  return $ BPM (fromIntegral beats)

cmdOctave = do
  octave <- natural
  return $ Octave (fromIntegral octave)

cmdSelect = do
  name <- stringLiteral
  return $ Select name

cmdLoop = do
  times <- natural
  cmds <- commandBlock
  return $ Loop times cmds

cmdPitch = do
  pitch <- identifier
  return $ Pitch Nothing pitch

cmdPlay = do
  notes <- natural
  return $ Play (fromIntegral notes)

cmdWait = do
  notes <- natural
  return $ Wait (fromIntegral notes)

command = do
  cmd <- identifier
  case cmd of
       "BPM" -> cmdBPM
       "Loop" -> cmdLoop
       "Octave" -> cmdOctave
       "Play" -> cmdPlay
       "Wait" -> cmdWait
       "Pitch" -> cmdPitch
       "Select" -> cmdSelect
       _ -> unexpected $ "command: " ++ cmd

commandBlock = braces $ many1 (lexeme command)

channel = do
  symbol "Channel"
  number <- natural
  cmds <- commandBlock
  return ((fromIntegral number), cmds)

music = do
  whiteSpace
  instruments <- many1 (lexeme namedInstrument)
  channels <- many1 (lexeme channel)
  return $ Music instruments channels

-- compiler part

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

instance Binary [Instruction] where
  put l = mapM_ put l

type Environment = [(String, Instrument)]

compileChannel :: [Command] -> [(String, Instrument)] -> [Instruction]
compileChannel l ctx = concat $ map (\e -> runReader (toInsn e) ctx) l

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
          ++ compileChannel cs ctx ++ [DecCtrBr, PopI BrR, PopI CtrR])
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

data Audio = Audio { frameRate :: Word16,
                     frames    :: Word32,
                     samples   :: [Word8] }
     deriving Show

instance Binary Audio where
  put audio = do
    put (frameRate audio)
    put (frames audio)
    mapM_ put (samples audio)

extractAudio :: WAVE -> Audio
extractAudio wave = Audio audioFrameRate audioFrames (map extract audioSamples)
  where header = waveHeader wave
        audioSamples = map head (waveSamples wave)
        audioFrameRate = fromIntegral $ waveFrameRate header
        audioFrames = case waveFrames header of
                           Just n -> fromIntegral n
                           Nothing -> error "Unknown frame number!"
        extract s = fromIntegral $ s `shiftR` 24

getSampleFile i = case snd i of
                       Sample filename -> filename

compile m = do
  samples <- mapM (getWAVEFile . getSampleFile) (instruments m)
  print $ map extractAudio samples

-- main part

main = do
  c <- getContents
  case parse music "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right m -> do print m
                  compile m
                  --B.writeFile "test.bin" (B.pack $ compile c)
