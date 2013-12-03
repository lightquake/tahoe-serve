import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text.Lazy                as T
import           Network.HTTP.Conduit
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Highlighting.Kate
import           Web.Scotty

main :: IO ()
main = scotty 16384 $
    get (regex "^/(URI:.*)$") $ do
        uri <- param $ T.pack "1"
        fileType <- (param $ T.pack "type") `rescue` const (return "raw")
        contents <- fmap (UTF8.toString . L.toStrict) . simpleHttp $
                    "http://localhost:6543/uri/" ++ uri
        serveAs fileType contents

serveAs :: String -> String -> ActionM ()
serveAs "raw" contents = text $ T.pack contents
serveAs fileType contents = html rendered
  where rendered = renderHtml $ do
            H.head $ H.style H.! A.type_ (H.toValue "text/css")
                $ toHtml $ styleToCss pygments
            H.body $ toHtml
                $ formatHtmlBlock defaultFormatOpts
                $ highlightAs fileType contents
