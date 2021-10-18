
-- Parses a product page on shop.gazzal.net

{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( showProductData
    ) where
import Text.HTML.Scalpel ( scrapeURL, text, (@:), (@=), Scraper, hasClass, (//), URL, chroots, attrs, attr, scrapeStringLike, fetchTagsWithConfig, scrape, texts, innerHTML, Config (Config), utf8Decoder )
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (join)
import Debug.Trace (trace)
import System.Environment ( getArgs )


trim :: [Char] -> [Char]
trim = f . f where f = reverse . dropWhile isSpace

baseURL = "https://shop.gazzal.net"
-- pageURL = "https://shop.gazzal.net/kategori/giza"
-- productPageURL = "https://shop.gazzal.net/urun/giza-2473"

type Images = [Maybe ImageData]
newtype ImageURL = ImageURL URL deriving Show
newtype BigImageURL = BigImageURL URL deriving Show
newtype ImageNumber = ImageNumber String deriving Show
data ImageData = ImageData ImageURL (Maybe BigImageURL) ImageNumber deriving Show
newtype ProductName = ProductName String deriving Show
newtype ProductMainDetails = ProductMainDetails String deriving Show
newtype Fibers = Fibers String deriving Show
data ProductData = ProductData (Maybe ProductName) (Maybe ProductMainDetails) (Maybe Fibers) (Maybe Images) deriving Show

showProductData :: IO ()
showProductData = do
    -- Should provice a command line argument - a product URL
    args <- getArgs
    f args
    where
        f :: [String] -> IO ()
        f args
            | length args == 1 = print =<< getProductData (head args)
            | otherwise = putStrLn "Wrong arguments number"

getProductData :: URL -> IO ProductData
getProductData url = do
    tags <- fetchTagsWithConfig (Config utf8Decoder Nothing) url
    let productName = getProductName tags
    let productDetail = getProductDetail tags
    let urls = getColorURLs tags
    ims <- getIms urls
    return $ getRes productName productDetail ims
    where
        getIms :: Maybe [URL] -> IO (Maybe Images)
        getIms urls = case urls of (Just a) -> Just <$> mapM getColorData a
                                   Nothing -> return Nothing 
        getRes productName productDetail ims =
            let (mDetail, fib) = case productDetail of (Just (a, b)) -> (Just a, Just b)
                                                       Nothing -> (Nothing, Nothing)
            in ProductData productName mDetail fib ims

getProductName = scrape res
    where
        res :: Scraper String ProductName
        res = do
            produtName <- text $ "div" @: ["id" @= "head-content"] // "h1" @: [hasClass "kategori"]
            return $ ProductName (trim produtName)

getProductDetail = scrape res
    where
        res :: Scraper String (ProductMainDetails, Fibers)
        res = do
            detailsStr <- text $ "div" @: [hasClass "kompozisyon"]
            let details = lines detailsStr
            let (fibs, mainDet) = getData details detailsStr
            return (ProductMainDetails mainDet, Fibers fibs)
                where
                    getData details detailsStr
                        | length details < 2 = error $ "less than 2 lines found in getProductDetail: " ++ detailsStr
                        | otherwise = (trim $ head details, trim $ details !! 1)

getColorURLs = scrape res
    where
        res :: Scraper String [URL]
        res = do
            urls <- attrs "href" $ "div" @: [hasClass "showcase-container"] // "a" @: [hasClass "showcase-label-container"]
            return $ map (baseURL ++) urls

getColorData :: URL -> IO (Maybe ImageData)
getColorData url = scrapeURL url res
    where
        res :: Scraper String ImageData
        res = do
            urls <- attrs "data-image" $ "div" @: ["id" @= "product-thumb-image"] // "a"
            let (url, bigUrl) = case urls of [_] -> getSingle urls
                                             _ -> getBoth urls
            num <- text $ "div" @: ["id" @= "product-detail-container"] // "div" @: [hasClass "product-right"] // "h1"
            return $ ImageData url bigUrl (ImageNumber $ trim num)
                where
                    getSingle urls = (ImageURL $ "https:" ++ head urls, Nothing)
                    getBoth urls = (ImageURL $ "https:" ++ urls !! 1, Just $ BigImageURL $ "https:" ++ head urls)
