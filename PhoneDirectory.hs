
module PhoneDirectory where 

import System.Directory

----- Part 1 Data Structures -----

type PhoneBook = [Person]
data Person = MKPE (Name, Phones, Address, DOB)
		deriving (Show)
type Name = [Char]
data Phones = MKPH (Mobile, Home, Work)
		deriving (Show)
type Mobile = [Char]
type Home = [Char]
type Work = [Char]
data Address = MKAD (Line1, Line2, Postcode, City)
		deriving (Show)
type Line1 = [Char]
type Line2 = [Char]
type Postcode = [Char]
type City = [Char]
type DOB = [Char]


--- PART 2  Write PhoneBook----
writePhoneBook :: [Person] -> [Char]
writePhoneBook [] = []
writePhoneBook xs = concat(map writePerson xs)

writePerson :: Person -> [Char]
writePerson (MKPE (n, p, a, d)) = "<person>\n" ++ (writeName n) ++ (writePhones p) ++ (writeAddress a) ++ (writeDOB d) ++ "</person>\n"

writePhones :: Phones -> [Char]
writePhones (MKPH (m, h, w)) = if m==[] && h==[] && w==[] then []
				else "\t<phones>\n" ++ (writeMobile m) ++ (writeHome h) ++ (writeWork w) ++ "\t</phones>\n"

writeAddress :: Address -> [Char]				
writeAddress (MKAD (x, y, p, c)) = if x==[] && y==[] && p==[] && c==[] then []
					else "\t<address>\n" ++ (writeLine1 x) ++ (writeLine2 y) ++ (writePostcode p) ++ (writeCity c) ++ "\t</address>\n"

writeName :: [Char] -> [Char]					
writeName n = "\t<name>"++n++"</name>\n"

writeDOB :: [Char] -> [Char]
writeDOB d = if (d==[]) then []
		else "\t<dob>"++d++"</dob>\n"

writeMobile :: [Char] -> [Char]		
writeMobile m = if (m==[]) then []
		else "\t\t<mobile>"++m++"</mobile>\n"

writeHome :: [Char] -> [Char]		
writeHome h = if (h==[]) then []
		else "\t\t<home>"++h++"</home>\n"

writeWork :: [Char] -> [Char]
writeWork w = if (w==[]) then []
		else "\t\t<work>"++w++"</work>\n"

writeLine1 :: [Char] -> [Char]		
writeLine1 x = if (x==[]) then []
		else "\t\t<line1>"++x++"</line1>\n"

writeLine2 :: [Char] -> [Char]		
writeLine2 y = if (y==[]) then []
		else "\t\t<line2>"++y++"</line2>\n"

writePostcode :: [Char] -> [Char]		
writePostcode p = if (p==[]) then []
		else "\t\t<postcode>"++p++"</postcode>\n"

writeCity :: [Char] -> [Char]		
writeCity c = if (c==[]) then []
		else "\t\t<city>"++c++"</city>\n"

----- Part 3 read PhoneBook -----

readPhoneBook :: [Char] -> [Person]
readPhoneBook [] = []
readPhoneBook ('<':'p':'e':'r':'s':'o':'n':'>':xs) = parsePerson xs:readPhoneBook (removeTillPerson(xs))
readPhoneBook (_:xs) = readPhoneBook xs

removeTillPerson :: [Char] -> [Char]
removeTillPerson [] = []
removeTillPerson ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = xs
removeTillPerson (_:xs) = removeTillPerson xs

parsePerson :: [Char] -> Person
parsePerson [] = MKPE ([], MKPH ([],[],[]), MKAD ([],[],[],[]), [])
parsePerson xs = MKPE (parseName xs, parsePhones xs, parseAddress xs, parseDOB xs)

parseName :: [Char] -> [Char]
parseName [] = []
parseName ('<':'n':'a':'m':'e':'>':xs) = takeWhile notOpening xs
parseName (_:xs) = parseName xs

notOpening c= not(c=='<')

parsePhones :: [Char] -> Phones
parsePhones [] = MKPH ([],[],[])
parsePhones xs = MKPH (parseMobile xs, parseHome xs, parseWork xs)

parseMobile :: [Char] -> [Char]
parseMobile [] = []
parseMobile ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseMobile ('<':'m':'o':'b':'i':'l':'e':'>':xs) = takeWhile notOpening xs
parseMobile (_:xs) = parseMobile xs

parseHome :: [Char] -> [Char]
parseHome [] = []
parseHome ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseHome ('<':'h':'o':'m':'e':'>':xs) = takeWhile notOpening xs
parseHome (_:xs) = parseHome xs

parseWork :: [Char] -> [Char]
parseWork [] = []
parseWork ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseWork ('<':'w':'o':'r':'k':'>':xs) = takeWhile notOpening xs
parseWork (_:xs) = parseWork xs

parseAddress :: [Char] -> Address
parseAddress [] = MKAD ([],[],[],[])
parseAddress xs = MKAD (parseLine1 xs, parseLine2 xs, parsePostcode xs, parseCity xs)

parseLine1 :: [Char] -> [Char]
parseLine1 [] = []
parseLine1 ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseLine1 ('<':'l':'i':'n':'e':'1':'>':xs) = takeWhile notOpening xs
parseLine1 (_:xs) = parseLine1 xs

parseLine2 :: [Char] -> [Char]
parseLine2 [] = []
parseLine2 ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseLine2 ('<':'l':'i':'n':'e':'2':'>':xs) = takeWhile notOpening xs
parseLine2 (_:xs) = parseLine2 xs

parsePostcode :: [Char] -> [Char]
parsePostcode [] = []
parsePostcode ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parsePostcode ('<':'p':'o':'s':'t':'c':'o':'d':'e':'>':xs) = takeWhile notOpening xs
parsePostcode (_:xs) = parsePostcode xs

parseCity :: [Char] -> [Char]
parseCity [] = []
parseCity ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseCity ('<':'c':'i':'t':'y':'>':xs) = takeWhile notOpening xs
parseCity (_:xs) = parseCity xs

parseDOB :: [Char] -> [Char]
parseDOB [] = []
parseDOB ('<':'/':'p':'e':'r':'s':'o':'n':'>':xs) = []
parseDOB ('<':'d':'o':'b':'>':xs) = takeWhile notOpening xs
parseDOB (_:xs) = parseDOB xs



------Part 4  Functions---------------------------------

--------Displaying the contact details of a given person (given the name) ----


   -- find function takes two parameters "name" and phoneBook" 
   -- displayContact takes two parameter .
--find :: Name -> [Person] -> [Char]
--find name phonebook =  clearify(concat(map (displayContact name) phonebook))  

getName (MKPE (n,_,_,_)) = n

find name phonebook = filter (\p -> (getName p) == name) phonebook

displayContact :: Name -> Person -> [Char]
displayContact name (MKPE (n, (MKPH (m, h, w)), (MKAD (x, y, p, c)), d)) 
      = if name == n then "Name: "++n++"Mobile :"++m
                     else ""

clearify :: [Char] -> [Char]
clearify xs = if xs == "" then "Not Found"
		else xs 

		
--------Displaying the whole contents of the XML file in a user-friendly way ----

prettyDisplay :: [Person] -> IO()
prettyDisplay xs = putStr(writePhoneBook xs)


--------Adding a new person to the XML file ----
    

addPerson :: [Char] -> IO()
addPerson path = do      c <- readFile path
			 let phonebook = readPhoneBook c
			 putStr "Enter Name"
			 n <- getLine
			 if n=="" then
				do
					putStr "Name Empty! Enter Name"
					addPerson path
			 	else do
					 putStr "Enter Mobile"
					 m <- getLine
		       			 putStr "Enter Home Phone"
					 h <- getLine
					 putStr "Enter Work Phone"
					 w <- getLine
					 putStr "Enter Address Line1"
					 x <- getLine
		       			 putStr "Enter Address Line2"
					 y <- getLine
					 putStr "Enter Postcode"
					 p <- getLine
					 putStr "Enter City"
					 c <- getLine
		       			 putStr "Enter DOB"
					 d <- getLine
					 let person = MKPE (n, (MKPH (m, h, w)), (MKAD (x, y, p, c)), d)
					 putStrLn ("Added Successfullty")
					 writeFile ("temp.xml") (writePhoneBook(person:phonebook))
					 removeFile path
					 renameFile "temp.xml" path 

--------Removing a person from the XML file ----

removePerson :: [Char] -> IO()
removePerson path = do 	 c <- readFile path
			 let phonebook = readPhoneBook c
			 putStr "Enter Name"
			 n <- getLine
			 writeFile ("temp.xml") (writePhoneBook (filter (removeFilter n) phonebook))
 			 removeFile path
			 renameFile "temp.xml" path

removeFilter :: Name -> Person -> Bool
removeFilter name (MKPE (n, (MKPH (m, h, w)), (MKAD (x, y, p, c)), d)) = not(name==n)


--------Adding phonenumber or address information of a given person ----

addPhone :: [Char] -> IO()
addPhone path = do 	 c <- readFile path
			 let phonebook = readPhoneBook c
			 putStr "Enter Name"
			 n <- getLine
			 putStr "Enter Mobile"
			 mm <- getLine
			 putStr "Enter Home Phone"
			 hh <- getLine
			 putStr "Enter Work Phone"
			 ww <- getLine
			 writeFile ("temp.xml") (writePhoneBook (map (changeDetails n mm hh ww) phonebook))
			 removeFile path
			 renameFile "temp.xml" path
			 
changeDetails :: Name -> Mobile -> Home -> Work -> Person -> Person
changeDetails name mm hh ww (MKPE (n, (MKPH (m, h, w)), (MKAD (x, y, p, c)), d)) = if name==n then (MKPE (n, (MKPH (mm, hh, ww)), (MKAD (x, y, p, c)), d))
										else (MKPE (n, (MKPH (m, h, w)), (MKAD (x, y, p, c)), d))


 

