import Test.Hspec
import Yeet.Parser

type ParseTestData = (String, Either String String)

parseTest :: ParseTestData -> Spec
parseTest (inp, out) = it ("Correctly parses `" ++ inp ++ "` to `" ++ (show out) ++ "` ") $ do
  (fmap show (parseYeet inp)) `shouldBe` out

parseTestCases :: [ParseTestData]
parseTestCases = [
    (
      "yeet1 yeeeetx yeet1 yeet2 yeeetf yeet2 yeeetf YEET2 yeet2 yeeetf yeet2 yeeetf YEET2 YEET1",
      Right "( λ x . ( λ f . f ) ( λ f . f ) )"
    ),
    (
      "yeet yeeet yeet yeet yeeeet yeet yeeet yeet yeet yeeeet yeeeet YEET YEET yeet yeeeet yeet yeeet yeet yeet yeeeet yeeeet YEET YEET YEET",
      Right "( λ f . ( λ x . f ( x x ) ) ( λ x . f ( x x ) ) )"
    ),
    (
      "",
      Left "1:1:\n  |\n1 | <empty line>\n  | ^\nunexpected end of input\nexpecting function\n"
    )
  ]

main :: IO ()
main = hspec $ do
  describe "Yeet.Parser" $ do
    mapM_ parseTest parseTestCases
