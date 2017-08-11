-- https://gist.github.com/creichert/4f8adbfcf165191e90c94c5bd1d2e4d9#file-fetch-url-hs-L27
-- stack -v runghc --package connection --package http-client --package http-client-tls --package tls --package data-default
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Zn.TLS (mkHttpManager) where

import qualified Network.Connection      as NC
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.TLS             as TLS
import qualified Network.TLS.Extra       as TLS (ciphersuite_all)
import qualified System.X509             as TLS

import Data.Default
import Data.Maybe
import Network.HTTP.Client
import System.Environment
import Prelude

main :: IO ()
main = do

    murl <- getArgs
    case listToMaybe murl of

        (fmap parseUrl -> Just (Just req)) -> do
            mgr <- mkHttpManager True
            res <- httpLbs req mgr
            print (show (responseStatus res))

        _ -> error "usage: fetch-url [URL] (must include http:// or https://"

-- | Create an HTTP 'Manager' for running a 'Test'
mkHttpManager :: Bool  -- ^ validate ssl
              -> IO Manager
mkHttpManager validateSsl = do

    scs <- TLS.getSystemCertificateStore
    let tlsSettings = NC.TLSSettings (cp scs)
        mngrCfg = Http.mkManagerSettings tlsSettings Nothing

    Http.newManager mngrCfg
  where
    cp scs = (TLS.defaultParamsClient "" "") {
                TLS.clientSupported = def {
                    TLS.supportedCiphers        = TLS.ciphersuite_all
                  , TLS.supportedHashSignatures = hashSignatures
                  -- , TLS.supportedVersions [TLS10, TLS11, TLS12]
                  }
              , TLS.clientShared = def {
                    TLS.sharedCAStore         = scs
                  , TLS.sharedValidationCache = validationCache
                  }
              }

    hashSignatures =
        [ (TLS.HashSHA512, TLS.SignatureRSA)
        , (TLS.HashSHA384, TLS.SignatureRSA)
        , (TLS.HashSHA256, TLS.SignatureRSA)
        , (TLS.HashSHA224, TLS.SignatureRSA)
        , (TLS.HashSHA1,   TLS.SignatureRSA)
        , (TLS.HashSHA1,   TLS.SignatureDSS)
        , (TLS.HashSHA512, TLS.SignatureECDSA) -- "bad SignatureECDSA for ecdhparams"
        , (TLS.HashSHA384, TLS.SignatureECDSA) -- "bad SignatureECDSA for ecdhparams"
        , (TLS.HashSHA256, TLS.SignatureECDSA)
        , (TLS.HashSHA224, TLS.SignatureECDSA)
        , (TLS.HashSHA1,   TLS.SignatureECDSA)
        ]

    validationCache =
        if not validateSsl then
            TLS.ValidationCache
                          (\_ _ _ -> return TLS.ValidationCachePass)
                          (\_ _ _ -> return ())
        else
            def
