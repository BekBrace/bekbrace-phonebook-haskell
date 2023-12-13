-- Phonebook version 2: adding functionality of saving data to an external file
import System.IO ( hFlush, stdout )

-- Define a data type Contact with fields name and phoneNumber
data Contact = Contact
  { name :: String
  , phoneNumber :: String
  } deriving (Show)

-- Define a type synonym for a list of contacts
type PhoneBook = [Contact]

-- Function to add a contact to the phone book
addContact :: PhoneBook -> Contact -> PhoneBook
addContact phoneBook contact = contact : phoneBook

-- Function to search for a contact by name in the phone book
searchContact :: PhoneBook -> String -> Maybe Contact
searchContact phoneBook query = 
  case filter (\c -> name c == query) phoneBook of
    [result] -> Just result
    _        -> Nothing

-- Function to display a contact as a string
displayContact :: Contact -> String
displayContact contact = name contact ++ ": " ++ phoneNumber contact

-- Function to display the entire phone book as a string
displayPhoneBook :: PhoneBook -> String
displayPhoneBook = unlines . map displayContact

-- Function to save the phone book to a file
savePhoneBookToFile :: FilePath -> PhoneBook -> IO ()
savePhoneBookToFile filePath phoneBook = writeFile filePath (displayPhoneBook phoneBook)

-- Function to load the phone book from a file
loadPhoneBookFromFile :: FilePath -> IO PhoneBook
loadPhoneBookFromFile filePath = do
  contents <- readFile filePath
  let contacts = map parseContact (lines contents)
  return contacts

-- Function to parse a line from a file into a Contact
parseContact :: String -> Contact
parseContact line = case words line of
  [name, phoneNumber] -> Contact name phoneNumber
  _                   -> error "Invalid line format in the file"

-- Function for the interactive loop
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

-- Main function to start the interactive loop with an empty phone book
main :: IO ()
main = interactiveLoop []
