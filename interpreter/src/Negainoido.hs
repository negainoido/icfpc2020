{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE RecordWildCards #-} 
module Negainoido(main) where

import qualified Data.Text as T
import Control.Monad.Identity
import GHC.Generics
import Control.Monad.Except
--import Debug.Trace
import qualified Data.Text.IO as T
import Data.Aeson(ToJSON(..), encodeFile)
import Negainoido.Syntax
import Negainoido.Eval
import Negainoido.Parser



main :: IO ()
main = do
    content <- T.getContents
    r <- runExceptT $ do
        let (gdef: defs) = reverse $ T.lines content 
        defs1 <- mapExceptT (pure . runIdentity) $ mapM parseDef defs
        mainExpr <- mapExceptT (pure . runIdentity) $ parseMain gdef
        rdata <- evalMain defs1 mainExpr
        liftIO $ putStrLn "raw output is written at result.txt"
        liftIO $ writeFile "result.txt" (show rdata)
        res@Result{..} <- dataToResult rdata
        liftIO $ putStrLn "result json is written at result.json"
        liftIO $ encodeFile "result.json" res
        liftIO $ putStrLn $ "Result: " ++ show returnValue
        liftIO $ T.putStrLn $ "DataAsCode: " <> toCode stateData
        case imageList of 
            Just imageList' -> 
                forM_ (zip [(1 :: Int)..] imageList')  $ \(i, image) -> do
                let filename = "image_" ++ show i ++ ".txt"
                    plot = unlines [ show x ++ " "  ++ show y | (x,y) <- image]
                liftIO $ putStrLn $ "image is written at " ++ filename
                liftIO $ writeFile filename plot
            Nothing -> pure ()
        when (returnValue /= 0) $ liftIO $ T.putStrLn $ "ImageListAsCode: " <> toCode imageListAsData
    case r of
        Left err -> putStrLn $ "Error: " ++ err
        Right () -> pure ()
    

data Result = Result {
    returnValue :: Integer,
    stateData :: SData,
    imageList :: Maybe [[(Integer, Integer)]],
    imageListAsData :: SData
} deriving(Generic, Show, Eq)

instance ToJSON Result

dataToResult :: SData -> ExceptT String IO Result
dataToResult (v1 `DCons` (v2 `DCons` (v3 `DCons` DNil))) = do
    n1 <- case v1 of  
        DNumber n -> pure n
        _ -> throwError $ "first element should be number" ++ show v1 
    let dat = v2
    imageList <- (Just <$> dataToImageList v3) `catchError` (\_ -> pure Nothing)
    pure Result {
        returnValue = n1, 
        stateData = dat,
        imageList = imageList,
        imageListAsData = v3 
        }
dataToResult e = throwError $ "Triplet is expected but found: " <> show e


dataToNumber :: SData -> ExceptT String IO Integer
dataToNumber (DNumber n) = pure n
dataToNumber e = throwError $ "expected number but found :" ++ show e

dataToImageList :: SData -> ExceptT String IO [[(Integer, Integer)]]
dataToImageList (DCons x xs) = (:) <$> dataToImage x <*> dataToImageList xs
dataToImageList DNil = pure []
dataToImageList e = throwError $ "expected nil or cons but found :" ++ show e

dataToImage :: SData -> ExceptT String IO [(Integer, Integer)]
dataToImage (DCons x xs) = (:) <$> dataToPoint x <*> dataToImage xs
dataToImage DNil = pure []
dataToImage e = throwError $ "expected nil or cons but found : " ++ show e

dataToPoint :: SData -> ExceptT String IO (Integer, Integer)
dataToPoint (DCons x y)  = do 
    n1 <- dataToNumber x
    n2 <- dataToNumber y
    pure (n1, n2)
dataToPoint e = throwError $ "expected point but found: " ++ show e
        
