{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Servant.Client
import Servant.API
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)
import System.Process (withCreateProcess, proc, CreateProcess)
import System.IO.Silently (silence)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Either (isRight)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Toxiproxy

main :: IO ()
main = do
  man <- newManager defaultManagerSettings
  let env = mkClientEnv man proxyUrl
  hspec $ do
    describe "Toxiproxy API" $ do
      it "get version" $
        withToxiproxyServer $
          run env getVersion `shouldReturn` Right version
      it "post reset" $
        withToxiproxyServer $
          run env postReset `shouldReturn` Right NoContent
      it "create, update, get and delete a proxy" $
        withToxiproxyServer $ do
          let name = "myProxy"
          let proxy = Proxy
                { proxyName     = name
                , proxyListen   = "127.0.0.1:4444"
                , proxyUpstream = "127.0.0.1:4445"
                , proxyEnabled  = False
                , proxyToxics   = []
                }
          run env (createProxy proxy) `shouldReturn` Right proxy
          run env getProxies `shouldReturn` Right (Map.fromList [(name, proxy)])
          run env (getProxy name) `shouldReturn` Right proxy
          let enabled = proxy { proxyEnabled  = True }
          run env (updateProxy name enabled) `shouldReturn` Right enabled
          run env (deleteProxy name) `shouldReturn` Right NoContent
          run env getProxies `shouldReturn` Right Map.empty
      it "populate proxies" $
        withToxiproxyServer $ do
          let proxy1 = Proxy
                { proxyName     = "myProxy"
                , proxyListen   = "127.0.0.1:4444"
                , proxyUpstream = "127.0.0.1:4445"
                , proxyEnabled  = False
                , proxyToxics   = []
                }
          let proxy2 = Proxy
                { proxyName     = "myOtherProxy"
                , proxyListen   = "127.0.0.1:4446"
                , proxyUpstream = "127.0.0.1:4447"
                , proxyEnabled  = False
                , proxyToxics   = []
                }
          run env (postPopulate [proxy1, proxy2]) `shouldReturn` Right (Populate [proxy1, proxy2])
      it "create get, update and delete toxic" $
        withToxiproxyServer $ do
          let name = "myProxy"
          let toxicName = "latency"
          let toxic = Toxic
                { toxicName       = toxicName
                , toxicType       = Latency
                , toxicStream     = Upstream
                , toxicToxicity   = 1
                , toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)]
                }
          let proxy = Proxy
                { proxyName     = name
                , proxyListen   = "127.0.0.1:4444"
                , proxyUpstream = "127.0.0.1:4445"
                , proxyEnabled  = False
                , proxyToxics   = []
                }
          let proxyWithToxic = proxy { proxyToxics = [toxic] }
          let updatedToxic =
                toxic { toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)] }
          run env (createProxy proxy) `shouldReturn` Right proxy
          run env (createToxic name toxic) `shouldReturn` Right toxic
          run env (getToxics name) `shouldReturn` Right [toxic]
          run env (getProxy name) `shouldReturn` Right proxyWithToxic
          run env (updateToxic name toxicName updatedToxic) `shouldReturn` Right updatedToxic
          run env (deleteToxic name toxicName) `shouldReturn` Right NoContent
          run env (getToxics name) `shouldReturn` Right []
    describe "Toxiproxy Helpers" $ do
      it "disabled temporarily using withDisabled" $
        withToxiproxyServer $ do
          let proxy = Proxy
                { proxyName     = "myProxy"
                , proxyListen   = "127.0.0.1:4444"
                , proxyUpstream = "127.0.0.1:8474"
                , proxyEnabled  = True
                , proxyToxics   = []
                }
          withProxy env proxy $ \proxy -> do
            runThroughProxy getVersion `shouldReturn` Right version
            withDisabled env proxy $ do
              resp <- runThroughProxy getVersion
              isRight resp `shouldBe` True
            runThroughProxy getVersion `shouldReturn` Right version
      it "has temporary toxic using withToxic" $
        withToxiproxyServer $ do
          let proxy = Proxy
                { proxyName     = "myProxy"
                , proxyListen   = "127.0.0.1:4444"
                , proxyUpstream = "127.0.0.1:8474"
                , proxyEnabled  = True
                , proxyToxics   = []
                }
          let toxic = Toxic
                { toxicName       = "latency"
                , toxicType       = Latency
                , toxicStream     = Upstream
                , toxicToxicity   = 1
                , toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)]
                }
          withProxy env proxy $ \proxy -> do
            runThroughProxy getVersion `shouldReturn` Right version
            withToxic env proxy toxic $ do
              before <- getPOSIXTime
              runThroughProxy getVersion `shouldReturn` Right version
              after <- getPOSIXTime
              after - before `shouldSatisfy` (>1)
            runThroughProxy getVersion `shouldReturn` Right version

withToxiproxyServer :: IO a -> IO a
withToxiproxyServer f =
  -- silence $
  withCreateProcess server $ \_ _ _ _ -> threadDelay 100000 >> f
  where
    server :: CreateProcess
    server = proc "toxiproxy-server" ["-port", "4444"]

version :: Version
version = Version "2.1.4"

proxyUrl :: BaseUrl
proxyUrl = BaseUrl Http "127.0.0.1" 4444 ""

runThroughProxy :: ClientM a -> IO (Either ClientError a)
runThroughProxy f = do
  manager <- newManager defaultManagerSettings
  runClientM f (mkClientEnv manager proxyUrl)
