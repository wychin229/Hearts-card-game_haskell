{- Write a report describing your design and strategy here.
 Name : Chin Wen Yuan
 Student ID : 29975239

 Design :
 - I saved all previous played card in each trick into "memory"
 - I update the "memory" at EACH trick
 - I check if Heart is broken by searching for 'H' in the "memory" at each trick

 Strategies :
 * Main Idea : Lose most tricks and NOT gain points.
 ** The priority of the condition goes from up to down (first is the most important ones)
 1) When I am the one leading a trick, I will prefer not to lead with Heart cards unless I am only left with it
    --> When its the first trick, I will play the Two of Club (the starting of first trick)
    --> Else its NOT the first trick,
        - I will prefer playing smallest(lowest rank) non-point cards (if any) --> This is to maximise my chance of not winning this trick
        - If, I am left with all point card and I have Queen of Spade in hand, I will play the Queen of Spade (*This is to avoid Breaking)
        - else, I will play the smallest Heart card (point card)
2) When I am NOT the one leading a trick,
   --> When its the first trick,
        - I have cards same suit as the leading suit (follow suit), I will renege and play those cards
        - I don't have cards that can follow suit, I will play the largest(highest rank) non-point card (*This is to avoid Bleeding)
   --> When its NOT the first trick,
        - If I have Queen of Spade in hand, I will try to play it. (I want to get rid of highest point card)
        - If I can follow suit, I will try to find a card from my hand that is smaller than the highest rank of leading suit in current trick, and
          play it. (I am trying to lose this trick)
        - If I cannot follow suit, Heart is NOT broken, and I have non-point cards, I will play the highest rank card. 
          (So I can get rid of the high ranking cards, and left with lower ranking cards only, this is because I hope to lose the next trick.)
        - If I cannot follow suit, Heart is NOT broken, but I am ONLY left with point cards, I will play the highest Heart card.
          (So that when the future leading suit is Heart, I can try to lose that trick and not gain points, and attempt to make my opponents take the point cards)
        - If I cannot follow suit, BUT I have Heart cards and Heart is broken, I will also play the highest Heart card.
          (So that when the future leading suit is Heart, I can try to lose that trick and not gain points, and attempt to make my opponents take the point cards)
        - If the above conditions are not met, I simply play the highest ranking card in my hand (no strategy to this.)

Condition of me selecting Queen of Spade when there are other choices of cards :
- When I am able to find a Spade card which has higher rank than Queen of Spade in the current trick(already played by other players)
- When the current leading suit is not Spade and I cannot follow suit. (This is me trying to get rid of the highest point card.)
-}
module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards
import Data.Maybe
import Data.List
import Hearts.Rules

playCard :: PlayFunc
-- | plays the lead (C2) when there is no trick played
playCard _ hand [] previous 
    | previous == Nothing = (lead hand, "") 
    -- | DO NOT PLAY SQ WHEN I AM LEADING
    -- | if i am leading , heart is not broken and i have other cards than Hearts
    | previous /= Nothing && haveNonPointCards hand == True 
        = (lowestRankCard (getNonPointCard hand), updateMemo previous)
    -- | if i m leading, heart is not broken, haveSQ
    | previous /= Nothing && haveNonPointCards hand == False && checkSQ hand == True
        = (Card Spade Queen, updateMemo previous)
    | previous /= Nothing && haveNonPointCards hand == False && checkSQ hand == False
        = (lowestRankCard hand, updateMemo previous)
-- | when i am following and not leading 
playCard _ hand trick previous
    -- | if previous is Nothing, it means that this is the first trick. Therefore i cannot play any POINT cards. (Bleeding Rule)
    | previous == Nothing && suitInHand hand (leadTrickSuit trick) == True
        = (highestRankCard (getSuitCard (getNonPointCard hand) (leadTrickSuit trick)), updateMemo previous)
    | previous == Nothing && suitInHand hand (leadTrickSuit trick) == False 
        = (highestRankCard (getNonPointCard hand), updateMemo previous)

    -- | When its not the FIRST trick,
    | previous /= Nothing && checkSQ hand == True && canPlaySQ trick hand == True 
        = ((Card Spade Queen), updateMemo previous)
    | previous /= Nothing && suitInHand hand (leadTrickSuit trick) == True 
        = (getHighestSmallerRank (getSuitCard hand (leadTrickSuit trick)) (filTrickHighRank (filTrickLeadSuit trick)), updateMemo previous)
    | previous /= Nothing && suitInHand hand (leadTrickSuit trick) == False && checkHeart previous == False && haveNonPointCards hand == True
        = (highestRankCard (getNonPointCard hand), updateMemo previous)
    | previous /= Nothing && suitInHand hand (leadTrickSuit trick) == False && checkHeart previous == False && haveNonPointCards hand == False
        = (highestRankCard (getSuitCard hand Heart), updateMemo previous)
    | previous /= Nothing && suitInHand hand (leadTrickSuit trick) == False && checkHeart previous == True && suitInHand hand Heart == True
        = (highestRankCard (getSuitCard hand Heart), updateMemo previous)
    | otherwise = (highestRankCard hand, updateMemo previous)
    
-- | getting the lead suit of this trick
leadTrickSuit :: [(Card, PlayerId)] -> Suit
leadTrickSuit = suit . fst . last

-- | get the cards that has the same suit as the leading in the current trick
filTrickLeadSuit :: [(Card, PlayerId)] -> [(Card, PlayerId)]
filTrickLeadSuit = filter =<< (. (suit . fst)) . (==) . leadTrickSuit

-- | filter the trick to get highest rank among cards in trick
filTrickHighRank :: [(Card, PlayerId)] -> Rank
filTrickHighRank trick = selectMax (head trick) (tail trick)
    where
        selectMax :: (Card, PlayerId) -> [(Card, PlayerId)] -> Rank
        selectMax firstP [] = rank (fst firstP)
        selectMax firstP (m:ms)
            | rank (fst firstP) < rank (fst m) = selectMax m ms
            | otherwise = selectMax firstP ms

-- | check if my hand has cards for that suit
suitInHand :: [Card] -> Suit -> Bool
suitInHand hand s = if filter ((s ==) . suit) hand /= ([]) then True else False

-- | check if I can play Spade Queen
canPlaySQ :: [(Card, PlayerId)] -> [Card] -> Bool
canPlaySQ trick hand 
    -- | check if there is a spade card which has higher rank than SQ is played in the trick
    | leadTrickSuit trick == Spade && filTrickHighRank (filTrickLeadSuit trick) > Queen = True
    -- | play SQ when the current leading is not Spade and I cannot follow suit
    | leadTrickSuit trick /= Spade && suitInHand hand (leadTrickSuit trick) == False = True 
    | otherwise = False

-- | check if Spade Queen is in hand
checkSQ :: [Card] -> Bool
checkSQ hand = if filter (Card Spade Queen ==) hand == ([]) then False else True

-- | check if I have non-point card
haveNonPointCards :: [Card] -> Bool 
haveNonPointCards hand = if getNonPointCard hand /= [] then True else False

-- | get the cards of that suit from hand
getSuitCard :: [Card] -> Suit -> [Card]
getSuitCard hand s = 
    if filter ((s ==) . suit) hand /= ([]) then filter ((s ==) . suit) hand
    else hand

-- | filter out point cards Hearts & Queen of Spade
getNonPointCard :: [Card] -> [Card]
getNonPointCard hand = filter (Card Spade Queen /=) (filter ((Heart /=) . suit) hand)

-- | this function is only called when i am leading to select a card when i am leading
lead :: [Card] -> Card
-- | if the Clubs of Two is in my hand i will play it (start of game)
lead hand = select (find (== (Card Club Two)) hand) where
    select :: Maybe Card -> Card
    -- | if i cant find Clubs of Two
    select Nothing
        | filter ((Heart /=) . suit) hand == ([]) = head (filter ((Heart ==) . suit) hand)
        | otherwise = head (filter ((Heart /=) . suit) hand)
    select card = fromJust card

-- | get the card that is smaller than the highest rank of leading suit in current trick
getHighestSmallerRank :: [Card] -> Rank -> Card
getHighestSmallerRank hand r 
    -- | I try not to follow suit with Queen of Spade (because I might ending up getting the points back)
    -- | if after filter SQ I don't have any card left and I have Queen of Spade (SQ), play SQ
    | filter (Card Spade Queen /=) hand == ([]) && checkSQ hand == True = Card Spade Queen
    -- | after filtering SQ, if I cant find any card that is smaller than the largest rank, it means that I only have same suit card that is all larger than the highest rank in trick
    | filter ((< r) . rank) hand == ([]) = lowestRankCard (filter (Card Spade Queen /=) hand)
    -- | if I can find cards that is smaller than the highest rank in trick, I will play the highest (but still smaller than the highest in trick)
    | otherwise = highestRankCard (filter (\x -> rank x < r) hand)

-- | get the highest ranked card given a list of cards of the same suit 
-- function recursively calls the next element in list to see if current element is bigger than current biggest card
highestRankCard :: [Card] -> Card
highestRankCard hand = maxCard (head hand) (tail hand)
    where 
        maxCard :: Card -> [Card] -> Card
        maxCard currentMax [] = currentMax
        maxCard card (m:ms)
            | rank card < rank m = maxCard m ms
            | otherwise = maxCard card ms

-- | get the smallest ranked card given a list of cards
-- function recursively calls the next element in list to see if current element is smaller than current smallest card
lowestRankCard :: [Card] -> Card
lowestRankCard hand = minCard (head hand) (tail hand)
    where
        minCard :: Card -> [Card] -> Card
        minCard currentMin [] = currentMin
        minCard card (m:ms)
            | rank card > rank m = minCard m ms 
            | otherwise = minCard card ms

-- | save the cards played in previous trick into memory
updateMemo :: Maybe ([(Card, PlayerId)], String) -> String
updateMemo mt = 
    case mt of 
        Just x -> listToString (fst x) ++ snd x
        Nothing -> ""

-- | convert the list of plays (trick) into String
listToString :: [(Card, PlayerId)] -> String
listToString l = foldr (++) "" (playToString <$> l) 
    where
        playToString :: (Card, PlayerId) -> String
        playToString (Card s r, _) = show s ++ show r

-- | check for Heart from previous state
checkHeart :: Maybe ([(Card, PlayerId)], String) -> Bool
checkHeart mt = 
    case mt of 
        Just x -> if filter ('H' ==) (snd x) /= ("") then True else False 
        Nothing -> False

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- | Given a card, select its rank.
rank :: Card -> Rank 
rank (Card _ r) = r 

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
