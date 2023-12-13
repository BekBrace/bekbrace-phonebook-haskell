-- phoneBook.hs
import System.IO

-- Define a Contact data type
data Contact = Contact
  { name :: String
  , phoneNumber :: String
  } deriving (Show)

-- Define the PhoneBook type
type PhoneBook = [Contact]

-- Function to add a contact to the phone book
addContact :: PhoneBook -> Contact -> PhoneBook
addContact phoneBook contact = contact : phoneBook

-- Function to search for a contact by name
searchContact :: PhoneBook -> String -> Maybe Contact
searchContact phoneBook query = 
  case filter (\c -> name c == query) phoneBook of
    [result] -> Just result
    _        -> Nothing

-- Function to display a contact
displayContact :: Contact -> String
displayContact contact = name contact ++ ": " ++ phoneNumber contact

-- Function to display the entire phone book
displayPhoneBook :: PhoneBook -> String
displayPhoneBook = unlines . map displayContact

-- Main interactive loop
interactiveLoop :: PhoneBook -> IO ()
interactiveLoop phoneBook = do
  putStrLn "Phone Book in Haskell"
  putStrLn "1. Add Contact"
  putStrLn "2. Search Contact"
  putStrLn "3. Display Phone Book"
  putStrLn "4. Exit"

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

    "4" -> putStrLn "Exiting."

    _   -> do
      putStrLn "Invalid option. Please choose again."
      interactiveLoop phoneBook

-- Entry point
main :: IO ()
main = interactiveLoop []

