-- Phonebook version 2 : adding functionality of saving data to external file
import System.IO ( hFlush, stdout )

data Contact = Contact
  { name :: String
  , phoneNumber :: String
  } deriving (Show)

type PhoneBook = [Contact]

addContact :: PhoneBook -> Contact -> PhoneBook
addContact phoneBook contact = contact : phoneBook

searchContact :: PhoneBook -> String -> Maybe Contact
searchContact phoneBook query = 
  case filter (\c -> name c == query) phoneBook of
    [result] -> Just result
    _        -> Nothing

displayContact :: Contact -> String
displayContact contact = name contact ++ ": " ++ phoneNumber contact

displayPhoneBook :: PhoneBook -> String
displayPhoneBook = unlines . map displayContact

savePhoneBookToFile :: FilePath -> PhoneBook -> IO ()
savePhoneBookToFile filePath phoneBook = writeFile filePath (displayPhoneBook phoneBook)

loadPhoneBookFromFile :: FilePath -> IO PhoneBook
loadPhoneBookFromFile filePath = do
  contents <- readFile filePath
  let contacts = map parseContact (lines contents)
  return contacts

parseContact :: String -> Contact
parseContact line = case words line of
  [name, phoneNumber] -> Contact name phoneNumber
  _                   -> error "Invalid line format in the file"

interactiveLoop :: PhoneBook -> IO ()
interactiveLoop phoneBook = do
  putStrLn "Phone Book in Haskell"
  putStrLn "1. Add Contact"
  putStrLn "2. Search Contact"
  putStrLn "3. Display Phone Book"
  putStrLn "4. Save Phone Book to File"
  putStrLn "5. Load Phone Book from File"
  putStrLn "6. Exit"

  putStr "Choose an option: "
  hFlush stdout
  option <- getLine

  case option of
    "1" -> do
      putStr "Enter name: "
      hFlush stdout
      name <- getLine
      putStr "Enter phone number: "
      hFlush stdout
      phoneNumber <- getLine
      let newContact = Contact name phoneNumber
      interactiveLoop (addContact phoneBook newContact)

    "2" -> do
      putStr "Enter name to search: "
      hFlush stdout
      query <- getLine
      case searchContact phoneBook query of
        Just contact -> putStrLn $ "Found: " ++ displayContact contact
        Nothing      -> putStrLn "Contact not found"
      interactiveLoop phoneBook

    "3" -> do
      putStrLn "Phone Book:"
      putStrLn $ displayPhoneBook phoneBook
      interactiveLoop phoneBook

    "4" -> do
      putStr "Enter file name to save: "
      hFlush stdout
      fileName <- getLine
      savePhoneBookToFile fileName phoneBook
      putStrLn $ "Phone Book saved to file: " ++ fileName
      interactiveLoop phoneBook

    "5" -> do
      putStr "Enter file name to load: "
      hFlush stdout
      fileName <- getLine
      loadedPhoneBook <- loadPhoneBookFromFile fileName
      putStrLn $ "Phone Book loaded from file: " ++ fileName
      interactiveLoop loadedPhoneBook

    "6" -> putStrLn "Exiting."

    _   -> do
      putStrLn "Invalid option. Please choose again."
      interactiveLoop phoneBook

main :: IO ()
main = interactiveLoop []

