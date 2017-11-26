module IOReaders where

type StdinReader = IO String
type FileReader = FilePath -> IO [String]

getCmdsFromStdin :: StdinReader
getCmdsFromStdin = getLine

getCmdsFromFile :: FileReader
getCmdsFromFile filePath = do
  content <- readFile filePath
  return $ lines content
