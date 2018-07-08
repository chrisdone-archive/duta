import Control.Concurrent.Async
import qualified Duta
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration" (do it "Run server" (withServer (pure ())))

withServer :: IO a -> IO a
withServer m = withAsync Duta.start (const m)
