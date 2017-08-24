module RunClientM where

import Servant.Client (
  ClientEnv(ClientEnv), BaseUrl(BaseUrl), Scheme(Http),
  client, runClientM, ClientM)
import Network.HTTP.Client (
  newManager, defaultManagerSettings)

import Control.Exception (
  throwIO)

run :: ClientM a -> IO a
run query = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8081 "")
  result <- runClientM query clientEnv
  case result of
    Left err ->
      throwIO err
    Right a ->
      return a


