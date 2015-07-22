module Main where

import Data.Set
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Applicative ((<$>))
import qualified Control.Applicative as AP
import Debug.Trace
import Prelude hiding (drop)
import Data.Text hiding (empty, length)
import qualified Data.Text as T
import Data.Text.Read
import Text.HTML.Scalpel (Scraper, (@:), (@=), text, chroots, scrapeURL, hasClass, chroot)

data LoopState = LS Int (Set Car)

data Car = Car {
      make :: Text
    , model :: Text
    , serial :: Int
    , year :: Int
    , price :: Int
    , milage :: Int
} deriving (Show, Eq, Ord)

mtrace msg = return () --trace msg $ return ()

hasId id = "id" @= id

url page = "http://bilasolur.is/SearchResults.aspx?page="
            ++ show page
            ++ "&id=790a6699-bb63-449e-88b0-4b231aa2eee9"


scrapeInt :: Int -> String -> Scraper Text Int
scrapeInt prefixLength idSelect = do
    value <- clearDots . drop prefixLength <$> text ("div" @: [hasId idSelect])
    (intValue, rest) <- either (const AP.empty) return $ decimal value
    return intValue
    where clearDots = T.filter (/= '.')

getMakeAndModel :: Scraper Text (Text, Text)
getMakeAndModel = do
    make <- text $ "span" @: [hasClass "carmake"]
    model' <- text "a"
    let model = strip . drop l $ model'
        l = T.length make
    return (make, model)

getSerial :: Scraper Text Int
getSerial = scrapeInt 11 "car_list_itembottom_right"
-- 11 == length "raðnúmer"

getYear :: Scraper Text Int
getYear = scrapeInt 9 "car_list_item4"
-- 9 == length "Árgerð"
-- TODO: ignoring nýskráning here from 'rest'

getPrice :: Scraper Text Int
getPrice = (*1000) <$> scrapeInt 6 "car_list_item3"
-- 6 == length "Verð"
-- price is listed in thousands of kr

getMilage :: Scraper Text Int
getMilage = (*1000) <$> scrapeInt 7 "car_list_item5"
-- 7 == length "Akstur"
-- milage is listed in thousands of km

car :: Scraper Text Car
car = do
    (make, model) <- chroot ("div" @: [hasId "car_list_item2"]) getMakeAndModel
    serial <- getSerial
    year <- getYear
    price <- getPrice
    milage <- getMilage
    mtrace $ show serial
    return $ Car make model serial year price milage

allCars :: Scraper Text [Car] 
allCars = chroots ("div" @: [hasClass "car_list_zebra"]) car
    
loop :: StateT LoopState IO (Set Car)
loop = do
    (LS page cars) <- get

    mtrace (url page)
    newCars' <- liftIO $ scrapeURL (url page) allCars
    let serials = Data.Set.map serial cars
        newCars = fromList . fromMaybe [] $ newCars'
        newSerials = Data.Set.map serial newCars
        subset = newSerials `isSubsetOf` serials
        cars' = newCars `union` cars
    mtrace $ show newCars'
    if not subset
        then do
            put $ LS (page + 1) cars'
            loop
        else
            return cars

main = do
    cars <- fmap toList $ evalStateT loop $ LS 1 empty
    putStrLn $ "found " ++ show (length cars) ++ " cars"
    mapM_ print cars
