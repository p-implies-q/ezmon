{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

{- |

Module      : Main.hs
Description : A utility for managing monitor layouts
Copyright   : (c) David Janssen
License     : UNLICENSE

Maintainer  : janssen.dhj@gmail.com
Stability   : unstable
Portability : non-portable (relies on sysfs)

This module contains a little utility that manages cycling through different
commands for particular monitor setups. Basically, it looks through
'/sys/class/drm' to discover what monitors (by EDID) are plugged into which
ports. The full list of (port, edid) values at any given time is hashed into a
unique identifier for any particular setup, and a file is associated with that
hash in the XdgConfig 'ezmon' directory.

The ezmon application can cycle through lines in that file and execute them. Any
kind of command is permitted, but the intended use is through lines of xrandr
commands to cycle through monitor setups across particular monitor layouts.

A simple command-line interface is included which allows cycling through
commands, resetting to the first command, creating and editing configuration
files for the current setup, and displaying some simple status information.

To keep track of cycling, the script stores the previous cycle-number and
layout-hash in two small files in the XdgData 'ezmon' directory. When these
files do not exist, the application should behave like it is starting a cycle.
If no configuration file exists for a particular layout, the default command is
"xrandr --auto"

-}

import           ClassyPrelude
import           Control.Exception.Safe  (catchIO)
import           Control.Monad.Extra     (andM, ifM)
import           Options.Applicative
import           System.Directory
import qualified System.Environment as E (getEnv)
import           System.FilePath
import           System.Process          (callCommand)

-- *** Define all the types ***

type Port      = FilePath          -- | A place where a monitor could be plugged
type MonitorID = Int               -- | Hash of a monitors EDID
type Device    = (Port, MonitorID) -- | A monitor plugged into a particular port
type Cycle     = Int               -- | Count of the position in a cycle
type Command   = String            -- | A shell command to run

-- | A collection of the relevant information when running
data RunEnv = RunEnv
  { rsDevices  :: [Device]
  , rsCycle    :: Cycle
  , rsRepeat   :: Bool
  , rsCommands :: [Command]
  } deriving (Eq, Show)

-- | Different tasks the script can be instructed to understake
data Task = Cycle
          | Reset
          | CreateConfig
          | ShowStatus
          | Edit
          deriving (Eq, Show)


-- *** Discovering and inspecting active ports and their connected monitors ***

-- | Return whether a Port has a monitor plugged into it or not
isConnected :: Port -> IO Bool
isConnected p = andM
  [ doesDirectoryExist p
  , doesFileExist status
  , ("connected\n" == ) <$> readFile status]
  where status = p </> "status"

-- | Return a list of all the monitors and ports they are plugged in to
getDevices :: IO [Device]
getDevices = connected >>= mapM getOne
  where
    connected    = allPorts >>= filterM isConnected
    getOne p     = (p,) <$> getMonitor p
    getMonitor p = hash <$> readFile (p </> "edid")
    allPorts     = map (drmDir </>) <$> listDirectory drmDir
      where drmDir = "/sys/class/drm"


-- *** Gathering information about the environment ***

-- | Save layout ID and cycle-number to drive
saveDat :: RunEnv -> IO ()
saveDat env = do
  dir <- getXdgDirectory XdgData "ezmon"
  createDirectoryIfMissing True dir
  writeFileUtf8 (dir </> "layout-id") (tshow . hash . rsDevices $ env)
  writeFileUtf8 (dir </> "cycle")     (tshow . rsCycle $ env)

-- | Try to read a stored variable from file and return it
readDat :: Read a => String -> IO (Maybe a)
readDat name = flip catchIO (const (return Nothing)) $ do
  dir <- getXdgDirectory XdgData "ezmon"
  txt <- readFileUtf8 (dir </> name)
  return $ readMay txt

-- | Get the file associated with the current set of devices
confFile :: [Device] -> IO FilePath
confFile ds = do
  fname <- getXdgDirectory XdgConfig ("ezmon" </> show (hash ds))
  createDirectoryIfMissing True (takeDirectory fname)
  return fname

-- | Load a configuration from file
getConf :: [Device] -> IO (Maybe [Command])
getConf ds = catchIO f $ const (return Nothing)
  where f = do fl <- confFile ds
               Just . lines . unpack <$> readFileUtf8 fl

-- | Gather all the relevant information about this run into a RunEnv record
getEnv :: IO RunEnv
getEnv = RunEnv <$> devs <*> cycl <*> reps <*> cmds
  where
    devs = getDevices
    cycl = fromMaybe 0  <$> readDat "cycle"
    cmds = fromMaybe [] <$> (devs >>= getConf)
    reps = do
      old <- readDat "layout-id"
      new <- hash <$> devs
      return $ maybe False (new ==) old


-- *** Running different commands based on instruction and env. ***

-- | The default command to run when no configuration file is encountered
defaultCmd :: Command
defaultCmd = "xrandr --auto"

-- | Run the next command in the list associated with this layout, if we just
-- switched layouts this is the same as reset, but if the layout hasn't
-- switched, this cycles through commands.
cycle :: RunEnv -> IO ()
cycle env = if not $ rsRepeat env then reset env else do
  callCommand cmd
  saveDat env { rsCycle = new }
  where
    cmd = fromMaybe defaultCmd (rsCommands env `index` new)
    idx = rsCycle env
    new = if idx >= length (rsCommands env) - 1 then 0 else idx + 1

-- | Run the first command in the list associated with this layout and set cycle to 0
reset :: RunEnv -> IO ()
reset env = do
  callCommand cmd
  saveDat env { rsCycle = 0 }
  where cmd = fromMaybe defaultCmd (rsCommands env `index` 0)

-- | Give a pretty representation of a RunEnv
pprintEnv :: RunEnv -> Text
pprintEnv env = unlines $
  ("Layout ID: " ++ (tshow . hash . rsDevices $ env)) : zipWith f [0..] (rsCommands env)
  where f i l = (if i == rsCycle env then "* " else "  ") ++ pack l



-- | Dispatch the specified task to a particular operation
run :: RunEnv -> Task -> IO ()
-- Cycle through commands
run env Cycle        = cycle env
-- Reset command
run env Reset        = reset env
-- Create an empty config file for the current layout if the file does not exist
run env CreateConfig = do
  cfg <- confFile $ rsDevices env
  ifM (doesFileExist cfg)
    (putStrLn $ "File already exists: " ++ pack cfg)
    (writeFileUtf8 cfg mempty)
-- Display some information about the current layout and configurations
run env ShowStatus   = putStrLn $ pprintEnv env
run env Edit         = do
  editor <- catch (E.getEnv "EDITOR") f
  fname  <- confFile $ rsDevices env
  callCommand $ unwords [editor, fname]
  where f :: IOException -> IO String
        f _ = return "nano"

-- *** Parsing command-line arguments ***

task :: Parser Task
task = subparser $ mconcat
  [ command "cycle" (info (pure Cycle) (progDesc "Cycle monitor configuration"))
  , command "reset" (info (pure Reset) (progDesc "Jump to first monitor configuration"))
  , command "mkconfig" (info (pure CreateConfig) (progDesc "Create config file for this layout"))
  , command "status"   (info (pure ShowStatus) (progDesc "Show the status for this layout"))
  , command "edit"     (info (pure Edit) (progDesc "Open the current config in an editor"))
  ]

opts :: ParserInfo Task
opts = info (task <**> helper)
  ( fullDesc
  <> progDesc "Handle monitor-layouts"
  <> header   "ezmon - a tool for handling different monitor layouts" )

main :: IO ()
main = do
  env  <- getEnv
  args <- customExecParser (prefs showHelpOnEmpty) opts
  run env args
