import Data.List (subsequences, permutations, sortOn, find)
import Text.Read (readMaybe)
import Control.Monad (foldM, foldM_)
import Data.Maybe (isJust, fromMaybe, maybeToList, listToMaybe)

------------------------- Merge sort

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

------------------------- Game world types

type Character = String
type Party     = [Character]

type Node      = Int
type Location  = String
type Map       = [(Node,Node)]

data Game      = Over
               | Game Map Node Party [Party]
  deriving (Eq,Show)

type Event     = Game -> Game


testGame :: Node -> Game
testGame i = Game [(0,1)] i ["Russell"] [[],["Brouwer","Heyting"]]


------------------------- Helper functions for party management 

-- Inserts a party into the list of parties, at a given node.
updateParties :: Party -> [Party] -> Int -> [Party]
updateParties p ps i
    | i <= length ps && i >= 0 = take i ps ++ [p] ++ drop (i + 1) ps
    | otherwise                = ps

-- Returns the game with a new player party.
setPlayerParty :: Game -> Party -> Game
setPlayerParty (Game m n _ ps) p = Game m n p ps

-- Returns the game with a new list of parties.
setParties :: Game -> [Party] -> Game
setParties (Game m n p _) ps = Game m n p ps

------------------------- Assignment 1: The game world

-- Returns all nodes directly connected to a given node.
connected :: Map -> Node -> [Node]
connected m n = [if i == n then j else i | (i, j) <- m, i == n || j == n]

-- Creates a connection between two nodes.
connect :: Node -> Node -> Map -> Map
connect i j m
    | (i, j) `elem` m || (j, i) `elem` m = m
    | otherwise                          = msort ((min i j, max i j) : m)

-- Removes the connection between two nodes.
disconnect :: Node -> Node -> Map -> Map
disconnect i j = filter (/= (i, j)) . filter (/= (j, i))

-- Adds all members of a given party to the players party.
add :: Party -> Event
add p Over              = Over
add p g@(Game _ _ p' _) = setPlayerParty g p''
  where
    p'' = p ++ p'

-- Adds all members of a given party to the party at a given node.
addAt :: Node -> Party -> Event
addAt n p Over              = Over
addAt n p g@(Game _ _ _ ps) = setParties g ps'
  where
    p'  = p ++ (ps !! n)
    ps' =  updateParties p' ps n

-- Adds all members of a given party to the party at the current node.
addHere :: Party -> Event
addHere p Over              = Over
addHere p g@(Game _ n _ ps) = setParties g ps'
  where
    p'  = p ++ (ps !! n)
    ps' = updateParties p' ps n

-- Removes all members of a given party from the players party.
remove :: Party -> Event
remove p Over              = Over
remove p g@(Game _ _ p' _) = setPlayerParty g p''
  where
    p'' = filter (`notElem` p) p'

-- Removes all members of a given party from the party at a given node.
removeAt :: Node -> Party -> Event
removeAt n p Over              = Over
removeAt n p g@(Game _ _ _ ps) = setParties g ps'
  where
    p'  = filter (`notElem` p) (ps !! n)
    ps' = updateParties p' ps n

-- Removes all members of a given party from the party at the current node.
removeHere :: Party -> Event
removeHere p Over              = Over
removeHere p g@(Game _ n _ ps) = setParties g ps'
  where
    p'  = filter (`notElem` p) (ps !! n)
    ps' = updateParties p' ps n

------------------------- Helper functions for UI and input handling

-- Prints a string prefixed with a line number and returns the next line number.
numberedPrint :: Int -> String -> IO Int
numberedPrint ln item = do
    putStrLn (show ln ++ " " ++ item)
    return (ln + 1)

-- Gets and returns user input.
promptUser :: IO String
promptUser = do
    putStr (prompt ++ " ")
    input <- getLine
    putStrLn ""
    return input

------------------------- Assignment 2: Dialogues

prompt = ">>"
line0  = "There is nothing we can do."


data Dialogue = Action  String  Event
              | Branch  (Game -> Bool) Dialogue Dialogue
              | Choice  String  [( String , Dialogue )]

testDialogue :: Dialogue
testDialogue = Branch ( isAtZero )
  (Choice "Russell: Let's get our team together and head to Error." [])
  (Choice "Brouwer: How can I help you?"
    [ ("Could I get a haircut?", Choice "Brouwer: Of course." [])
    , ("Could I get a pint?",    Choice "Brouwer: Of course. Which would you like?"
      [ ("The Segmalt.",     Action "" id)
      , ("The Null Pinter.", Action "" id)]
      )
    , ("Will you join us on a dangerous adventure?", Action "Brouwer: Of course." (add ["Brouwer"] . removeHere ["Brouwer"]))
    ]
  )
 where
  isAtZero Over           = False
  isAtZero (Game _ n _ _) = n == 0

-- Carries out a dialogue, given the user input(s) if needed.
dialogue :: Game -> Dialogue -> IO Game
dialogue g (Action str event) = putStrLn str >> return (event g)
dialogue g (Branch cond t f)  = dialogue g (if cond g then t else f)
dialogue g (Choice str [])    = putStrLn str >> return g
dialogue g (Choice str opts)  = do
    putStrLn str
    foldM_ numberedPrint 1 (map fst opts)
    processInput
  where
    -- Carries out the corresponding dialogue choice for the user input
    processInput :: IO Game
    processInput = do
        input <- promptUser
        let num = readMaybe input :: Maybe Int
        case num of
          Just n
              | n > 0 && n <= length opts -> dialogue g (snd $ opts !! (n - 1))
              | n == 0                    -> return Over
          _                               -> putStrLn line6 >> processInput

-- Returns the dialogue for a given party.
findDialogue :: Party -> Dialogue
findDialogue p = fromMaybe defaultDlg dlg
  where
    dlg        = listToMaybe [ d | perm   <- permutations p 
                                 , Just d <- [lookup perm theDialogues]]
    defaultDlg = Action line0 id


------------------------- Assignment 3: The game loop

line1 = "You are in "
line2 = "You can travel to:"
line3 = "With you are:"
line4 = "You can see:"
line5 = "What will you do?"


-- Updates the game state based on user inputs to either travel or talk.
step :: Game -> IO Game
step Over              = return Over
step g@(Game m n p ps) = do
    putStr line1
    putStrLn (theDescriptions !! n)
    let dests = connected m n
    destCount <- printDestinations dests
    let opts = zip [destCount..] (p ++ (ps !! n))
    nextLine <- printPlayerParty p destCount
    printLocationParty (ps !! n) nextLine

    let
      -- Carries out the corresponding choice (travel or dialogue) for the user input.
      handleInput :: IO Game
      handleInput = do
          input <- promptUser
          let nums = mapM readMaybe (words input) :: Maybe [Int]
          case nums of
            Just nums
                | invalidInput -> putStrLn line6 >> handleInput
                | nums == [0]  -> return Over
                | trvlOpt      -> return (Game m n' p ps)
                | dlgOpt       -> dialogue g (findDialogue p')
              where
                i            = head nums
                invalidInput = null nums || i < 0
                trvlOpt      = (destCount - 1 >= i) && length nums == 1
                n'           = dests !! (i - 1)
                dlgOpt       = all (>= destCount) nums && not (null p')
                opts'        = filter (\(i, _) -> i `elem` nums) opts
                p'           = map snd opts'

            _                  -> putStrLn line6 >> handleInput

    putStrLn line5
    handleInput

  where
    -- Prints each travel option prefixed with a line number and returns the next line number.
    printDestinations :: [Node] -> IO Int
    printDestinations []    = return 1
    printDestinations dests = do
        putStrLn line2
        foldM numberedPrint 1 (map (theLocations !!) dests)

    -- Prints each player party member prefixed with a line number and returns the next line number.
    printPlayerParty :: Party -> Int -> IO Int
    printPlayerParty [] destCount = return destCount
    printPlayerParty p destCount  = do
        putStrLn line3
        foldM numberedPrint destCount p

    -- Prints each current location party member prefixed with a line number.
    printLocationParty :: Party -> Int -> IO ()
    printLocationParty [] _       = return ()
    printLocationParty p nextLine = do
        putStrLn line4
        foldM_ numberedPrint nextLine p

-- Continuously updates the game state based on user input until an Over state.
game :: IO ()
game = gameLoop start
  where
    gameLoop :: Game -> IO ()
    gameLoop Over = return ()
    gameLoop s    = do
        s' <- step s
        gameLoop s'


------------------------- Assignment 4: Safety upgrades

line6 = "[Unrecognized input]"


------------------------- Assignment 5: Solving the game

data Command  = Travel [Int] | Select Party | Talk [Int]
  deriving Show

type Solution = [Command]

-- Returns all dialogue paths ending in an action.
talk :: Game -> Dialogue -> [(Game, [Int])]
talk g (Action _ event)  = [(event g, [])]
talk g (Branch cond t f) = talk g $ if cond g then t else f
talk g (Choice _ opts)   = concatMap exploreOption (zip [1..] opts)
  where
    exploreOption :: (Int, (String, Dialogue)) -> [(Game, [Int])]
    exploreOption (i, (_, nextDlg)) = [(g', i : path) | (g', path) <- talk g nextDlg]

-- Returns all possible ways of starting a dialogue at the given location.
select :: Game -> [Party]
select (Game _ n p ps) = subsequences chars
  where
    p'    = ps !! n
    chars = p ++ p'

-- Uses breadth-first search to return the path of user inputs to each reachable node.
travel :: Map -> Node -> [(Node, [Int])]
travel m start = sortOn (length . snd) (bfs [(start, [])] [])
  where
    bfs :: [(Node, [Int])] -> [Node] -> [(Node, [Int])]
    bfs [] _            = []
    bfs ((n, path):queue) seen
        | n `elem` seen = bfs queue seen
        | otherwise     = (n, path) : bfs (queue ++ nextSteps n path seen) (n : seen)

    nextSteps :: Node -> [Int] -> [Node] -> [(Node, [Int])]
    nextSteps n path seen = [(next, path ++ [i])
                            | (i, next) <- zip [1..] (connected m n)
                            , next `notElem` seen
                            ]

-- Returns the choices made from the current game state to reach an action, and the resulting game.
allSteps :: Game -> [(Solution, Game)]
allSteps g@(Game m n p ps)
    | g == Over = []
    | otherwise =
      [ ([Travel trvlPath, Select p'', Talk dlgPath], g'')
      | (n', trvlPath) <- travel m n
      , let g'          = Game m n' p ps
      , p'             <- select g'
      , p''            <- getValidPermutation p'
      , (g'', dlgPath) <- talk g' (findDialogue p'')
      ]
  where
    -- Returns the permutation of the party that exists in theDialogues. 
    getValidPermutation :: Party -> [Party]
    getValidPermutation p = maybeToList $ find (isJust . (`lookup` theDialogues)) (permutations p)

-- Returns the choices made to reach Over state via dialogue options (solves the game).
solve :: Game -> Solution
solve Over = []
solve g    = case allSteps g of
    []                -> []
    (nextSteps, g'):_ -> nextSteps ++ solve g'

walkthrough :: IO ()
walkthrough = (putStrLn . unlines . filter (not . null) . map format . solve) start
  where
    format (Travel []) = ""
    format (Travel xs) = "Travel: " ++ unwords (map show xs)
    format (Select xs) = "Select: " ++ foldr1 (\x y -> x ++ ", " ++ y) xs
    format (Talk   []) = ""
    format (Talk   xs) = "Talk:   " ++ unwords (map show xs)


------------------------- Game data

start :: Game
start = Game theMap 0 [] theCharacters

theMap :: Map
theMap = [(1,2),(1,6),(2,4)]

theLocations :: [Location]
theLocations =
  -- Logicester
  [ "Home"           -- 0
  , "Brewpub"        -- 1
  , "Hotel"          -- 2
  , "Hotel room n+1" -- 3
  , "Temple"         -- 4
  , "Back of temple" -- 5
  , "Takeaway"       -- 6
  , "The I-50"       -- 7
  ]

theDescriptions :: [String]
theDescriptions =
  [ "your own home. It is very cosy."
  , "the `Non Tertium Non Datur' Brewpub & Barber's."
  , "the famous Logicester Hilbert Hotel & Resort."
  , "front of Room n+1 in the Hilbert Hotel & Resort. You knock."
  , "the Temple of Linearity, Logicester's most famous landmark, designed by Le Computier."
  , "the back yard of the temple. You see nothing but a giant pile of waste paper."
  , "Curry's Indian Takeaway, on the outskirts of Logicester."
  , "a car on the I-50 between Logicester and Computerborough. The road is blocked by a large, threatening mob."
  ]

theCharacters :: [Party]
theCharacters =
  [ ["Bertrand Russell"]                    -- 0  Home
  , ["Arend Heyting","Luitzen Brouwer"]     -- 1  Brewpub
  , ["David Hilbert"]                       -- 2  Hotel
  , ["William Howard"]                      -- 3  Hotel room n+1
  , ["Jean-Yves Girard"]                    -- 4  Temple
  , []                                      -- 5  Back of temple
  , ["Haskell Curry", "Jean-Louis Krivine"] -- 6  Curry's takeaway
  , ["Gottlob Frege"]                       -- 7  I-50
  ]

theDialogues :: [(Party,Dialogue)]
theDialogues = let
  always _ = True
  end str  = Choice str []
  isconn  _ _  Over           = False
  isconn  i j (Game m _ _ _ ) = elem i (connected m j)
  here         Over           = 0
  here        (Game _ n _ _ ) = n
  inParty   _  Over           = False
  inParty   c (Game _ _ p _ ) = elem c p
  isAt    _ _  Over           = False
  isAt    n c (Game _ _ _ ps) = elem c (ps !! n)
  updateMap _  Over           = Over
  updateMap f (Game m n p ps) = Game (f m) n p ps
 in
  [ ( ["Russell"] , Choice "Russell: Let's go on an adventure!"
      [ ("Sure." , end "You pack your bags and go with Russell.")
      , ("Maybe later.", end "Russell looks disappointed.")
      ]
    )
  , ( ["Heyting","Russell"] , end "Heyting: Hi Russell, what are you drinking?\nRussell: The strong stuff, as usual." )
  , ( ["Bertrand Russell"] , Branch (isAt 0 "Bertrand Russell") ( let
      intro = "A tall, slender, robed character approaches your home. When he gets closer, you recognise him as Bertrand Russell, an old friend you haven't seen in ages. You invite him in.\n\nRussell: I am here with a important message. The future of Excluded-Middle Earth hangs in the balance. The dark forces of the Imperator are stirring, and this time, they might not be contained.\n\nDo you recall the artefact you recovered in your quest in the forsaken land of Error? The Loop, the One Loop, the Loop of Power? It must be destroyed. I need you to bring together a team of our finest Logicians, to travel deep into Error and cast the Loop into lake Bottom. It is the only way to terminate it."
      re1   = ("What is the power of the Loop?" , Choice "Russell: for you, if you put it on, you become referentially transparent. For the Imperator, there is no end to its power. If he gets it in his possession, he will vanquish us all." [re2])
      re2   = ("Let's go!" , Action "Let's put our team together and head for Error." (updateMap (connect 1 0) . add ["Bertrand Russell"] . removeHere ["Bertrand Russell"]) )
      in Choice intro [re1,re2]
      ) ( Branch ( (==7).here) (end "Russell: Let me speak to him and Brouwer."
      ) (end "Russell: We should put our team together and head for Error." ) )
    )
  , ( ["Arend Heyting"] , Choice "Heyting: What can I get you?"
      [ ( "A pint of Ex Falso Quodbibet, please." , end "There you go." )
      , ( "The Hop Erat Demonstrandum, please."   , end "Excellent choice." )
      , ( "Could I get a Maltus Ponens?"          , end "Mind, that's a strong one." )
      ]
    )
  , ( ["Luitzen Brouwer"] , Branch (isAt 1 "Luitzen Brouwer")
      ( Choice "Brouwer: Haircut?"
        [ ( "Please." , let
          intro = "Brouwer is done and holds up the mirror. You notice that one hair is standing up straight."
          r1 i  = ( "There's just this one hair sticking up. Could you comb it flat, please?" , d i)
          r2    = ( "Thanks, it looks great." , end "Brouwer: You're welcome.")
          d  i  | i == 0    = Choice intro [r2]
                | otherwise = Choice intro [r1 (i-1),r2]
        in d 100)
        , ( "Actually, could you do a close shave?" , end "Of course. I shave everyone who doesn't shave themselves." )
        , ( "I'm really looking for help." , Choice "Brouwer: Hmmm. What with? Is it mysterious?"
          [ ( "Ooh yes, very. And dangerous." , Action "Brouwer: I'm in!" (add ["Luitzen Brouwer"] . removeHere ["Luitzen Brouwer"]) )
          ] )
        ]
      )
      ( end "Nothing" )
    )
  , ( ["David Hilbert"] , Branch (not . isconn 2 3) (let
        intro = "You wait your turn in the queue. The host, David Hilbert, puts up the first guest in Room 1, and points the way to the stairs.\n\nYou seem to hear that the next couple are also put up in Room 1. You decide you must have misheard. It is your turn next.\n\nHilbert: Lodging and breakfast? Room 1 is free."
        re1   = ("Didn't you put up the previous guests in Room 1, too?" , Choice "Hilbert: I did. But everyone will move up one room to make room for you if necessary. There is always room at the Hilbert Hotel & Resort." [("But what about the last room? Where do the guests in the last room go?" , Choice "Hilbert: There is no last room. There are always more rooms." [("How can there be infinite rooms? Is the hotel infinitely long?" , Choice "Hilbert: No, of course not! It was designed by the famous architect Zeno Hadid. Every next room is half the size of the previous." [re2])])])
        re2   =  ("Actually, I am looking for someone." , Action "Hilbert: Yes, someone is staying here. You'll find them in Room n+1. Through the doors over there, up the stairs, then left." (updateMap (connect 2 3)))
      in Choice intro [re1,re2]
      ) (end "Hilbert seems busy. You hear him muttering to himself: Problems, problems, nothing but problems. You decide he has enough on his plate and leave." )
    )
  , ( ["William Howard"] ,  Branch (isAt 3 "William Howard")
      (Choice "Howard: Yes? Are we moving up again?" [("Quick, we need your help. We need to travel to Error." , Action "Howard: Fine. My bags are packed anyway, and this room is tiny. Let's go!" (add ["William Howard"] . removeAt 3 ["William Howard"]))]
      ) (Branch (isAt 6 "William Howard") (Choice "Howard: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Howard: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        ]
      ) (end "Howard: We need to find Curry. He'll know the way.")
    ) )
  , ( ["Jean-Yves Girard"] , Branch (isconn 4 5)  (end "You have seen enough here.") (Action "Raised on a large platform in the centre of the temple, Girard is preaching the Linearity Gospel. He seems in some sort of trance, so it is hard to make sense of, but you do pick up some interesting snippets. `Never Throw Anything Away' - you gather they must be environmentalists - `We Will Solve Church's Problems', `Only This Place Matters'... Perhaps, while he is speaking, now is a good time to take a peek behind the temple..." (updateMap (connect 4 5) ))
    )
  , ( ["Vending machine"] , Choice "The walls of the Temple of Linearity are lined with vending machines. Your curiosity gets the better of you, and you inspect one up close. It sells the following items:"
      [ ( "Broccoli"  , end "You don't like broccoli." )
      , ( "Mustard"   , end "It might go with the broccoli." )
      , ( "Watches"   , end "They seem to have a waterproof storage compartment. Strange." )
      , ( "Camels"    , end "You don't smoke, but if you did..." )
      , ( "Gauloises" , end "You don't smoke, but if you did..." )
      ]
    )
  , ( ["Jean-Louis Krivine"] , end "Looking through the open kitchen door, you see the chef doing the dishes. He is rinsing and stacking plates, but it's not a very quick job because he only has one stack. You also notice he never passes any plates to the front. On second thought, that makes sense - it's a takeaway, after all, and everything is packed in cardboard boxes. He seems very busy, so you decide to leave him alone."
    )
  , ( ["Haskell Curry"] , Branch (isAt 6 "Haskell Curry")
      (Choice "Curry: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Curry: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        , ("Actually, I am looking for help getting to Error." , end "Curry: Hmm. I may be able to help, but I'll need to speak to William Howard.")
        ]
      ) (end "Nothing")
    )
  , ( ["Haskell Curry","William Howard"] , Branch (not . isconn 6 7) (Action "Curry:  You know the way to Error, right?\nHoward: I thought you did?\nCurry:  Not really. Do we go via Computerborough?\nHoward: Yes, I think so. Is that along the I-50?\nCurry:  Yes, third exit. Shall I go with them?\nHoward: Sure. I can watch the shop while you're away." (add ["Haskell Curry"] . removeAt 6 ["Haskell Curry"] . addAt 6 ["William Howard"] . remove ["William Howard"] . updateMap (connect 6 7) )) (end "It's easy, just take the third exit on I-50.")
    )
  , ( ["Gottlob Frege"] , end "A person who appears to be the leader of the mob approaches your vehicle. When he gets closer, you recognise him as Gottlob Frege. You start backing away, and he starts yelling at you.\n\nFrege: Give us the Loop! We can control it! We can wield its power!\n\nYou don't see a way forward. Perhaps Russell has a plan." )
  , ( ["Bertrand Russell","Gottlob Frege","Luitzen Brouwer"] , let
        intro = "Frege is getting closer, yelling at you to hand over the Loop, with the mob on his heels, slowly surrounding you. The tension in the car is mounting. But Russell calmly steps out to confront Frege.\n\nRussell:"
        re1   = ( "You cannot control its power! Even the very wise cannot see all ends!" , Choice "Frege: I can and I will! The power is mine!\n\nRussell:" [re2,re3] )
        re2   = ( "Brouwer, whom do you shave?" , Choice "Brouwer: Those who do not shave themselves. Obviously. Why?\n\nRussell:" [re3] )
        re3   = ( "Frege, answer me this: DOES BROUWER SHAVE HIMSELF?" , Action
                  "Frege opens his mouth to shout a reply. But no sound passes his lips. His eyes open wide in a look of bewilderment. Then he looks at the ground, and starts walking in circles, muttering to himself and looking anxiously at Russell. The mob is temporarily distracted by the display, uncertain what is happening to their leader, but slowly enclosing both Frege and Russell. Out of the chaos, Russell shouts:\n\nDRIVE, YOU FOOLS!\n\nYou floor it, and with screeching tires you manage to circle around the mob. You have made it across.\n\nEND OF ACT 1. To be continued..."
                  (const Over)
                )
      in Choice intro [re1,re2,re3]
    )
  , ( ["Bertrand Russell","Haskell Curry","Luitzen Brouwer"] , Branch ((==7).here) (end "Road trip! Road trip! Road trip!") (end "Let's head for Error!")
    )
  ]

