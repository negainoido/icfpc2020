{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE RecordWildCards #-} 
module Negainoido(main, mainApi) where

import qualified Data.Text as T
import Control.Monad.Identity
import GHC.Generics
import Control.Monad.Except
--import Debug.Trace
import qualified Data.Text.IO as T
import Data.Text(Text)
import Data.Maybe
import Data.Aeson(ToJSON(..), FromJSON(..), encode, decode')
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment

import Negainoido.Syntax
import Negainoido.Eval
import Negainoido.Parser

import Paths_interpreter

mainApi :: IO ()
mainApi = do
    galaxyPath <- getDataFileName "galaxy.txt"
    galaxyContent <- T.readFile galaxyPath
    port <- fromMaybe 8080. fmap read <$> lookupEnv "PORT"
    token <- fromMaybe "" <$> lookupEnv "TOKEN"

    let r = runExcept $ do
            let (gdef: defs) = reverse (T.lines galaxyContent) 
            defExprList <- mapM parseDef defs
            mainExpr <- parseMain gdef
            pure Context{ 
                galaxyDefs = defExprList, 
                galaxyMain = mainExpr,
                secretToken = token }
    case r of
        Left err -> putStrLn $ "error:" ++ err
        Right ctx -> Warp.run port (mainApp ctx)

data Galaxy = Galaxy { 
    galaxyState :: Text,
    galaxyArg   :: Text
} deriving(Eq, Show, Generic)

data Context = Context {
    galaxyDefs :: [Def],
    galaxyMain :: Expr,
    secretToken :: String
}

data ErrorResult = ErrorResult {
    errorMessage :: String
} deriving(Generic)

instance ToJSON (ErrorResult)
instance FromJSON Galaxy

mainApp :: Context -> Application
mainApp ctx req respond = do
    reqBody <- strictRequestBody req

    r <- runExceptT $ do
        galaxy <- case decode' reqBody of
            Nothing -> throwError "invalid request body"
            Just v -> pure v
        stateExpr <- 
            mapExceptT (pure . runIdentity) 
             $ parseMain (galaxyState galaxy)
        argExpr <-
            mapExceptT (pure . runIdentity)
             $ parseMain (galaxyArg galaxy)
        let defs = d1 : d2 : galaxyDefs ctx
            d1 = Def (NT 2000) argExpr
            d2 = Def (NT 2001) stateExpr
        rdata <- evalMain defs (galaxyMain ctx)
        result <- dataToResult rdata
        pure result
    case r of
        Left err -> do
            let headers = [("Content-Type", "application/json")]
                result = ErrorResult err
            respond $ responseLBS status200 headers (encode result)
        Right result -> do
            let headers = [("Content-Type", "application/json")]
            respond $ responseLBS status200 headers (encode result)


main :: IO ()
main = do
    content <- T.getContents
    r <- runExceptT $ do
        let (gdef: defs) = reverse $ T.lines content 
        defs1 <- mapExceptT (pure . runIdentity) $ mapM parseDef defs
        mainExpr <- mapExceptT (pure . runIdentity) $ parseMain gdef
        rdata <- evalMain defs1 mainExpr
        {-
        liftIO $ T.hPutStrLn stderr "raw output is written at result.txt"
        liftIO $ writeFile "result.txt" (show rdata)
        -}
        res@Result{..} <- dataToResult rdata
        --liftIO $ putStrLn "result json is written at result.json"
        --liftIO $ encodeFile "result.json" res
        liftIO $ hPutStrLn stderr $ "Result: " ++ show returnValue
        liftIO $ T.hPutStrLn stderr $ "DataAsCode: " <> toCode stateData
        liftIO $ B.putStrLn $ encode res
        {-
        case imageList of 
            Just imageList' -> 
                forM_ (zip [(1 :: Int)..] imageList')  $ \(i, image) -> do
                let filename = "image_" ++ show i ++ ".txt"
                    plot = unlines [ show x ++ " "  ++ show y | (x,y) <- image]
                liftIO $ putStrLn $ "image is written at " ++ filename
                liftIO $ writeFile filename plot
            Nothing -> pure ()
        when (returnValue /= 0) $ liftIO $ T.putStrLn $ "ImageListAsCode: " <> toCode imageListAsData
        -}
    case r of
        Left err -> hPutStrLn stderr $ "Error: " ++ err
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
        
