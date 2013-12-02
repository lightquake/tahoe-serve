import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as E
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
        contents <- fmap (T.unpack . E.decodeUtf8) . simpleHttp $
                    "http://localhost:6543/uri/" ++ uri
        let rendered = renderHtml $ do
                H.head $ H.style H.! A.type_ (H.toValue "text/css")
                    $ toHtml $ styleToCss pygments
                H.body $ toHtml
                    $ formatHtmlBlock defaultFormatOpts
                    $ highlightAs "haskell" contents
        html rendered
