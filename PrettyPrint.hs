{-# OPTIONS_GHC -Wall #-}

{-
  A pretty-printing library based on 'Text.PrettyPrint.HughesPJ'
  that incorporates foreground colors, background colors, and
  text styles.
-}

-- * Use http://hackage.haskell.org/package/pretty-terminal-0.1.0.0/docs/System-Console-Pretty.html
--       http://hackage.haskell.org/package/pretty-terminal
-- TODO: Give credit where credit is due
-- TODO: Have a function to check whether the terminal supports colors


module PrettyPrint
( -- All combinators I liked from PrettyPrint.HughesPJ
  (<>), (<+>), ($$), ($+$)
, hcat, hsep, vcat, sep, cat, fsep, fcat
, parens, brackets, braces, quotes, dQuotes
, maybeParens, maybeBrackets, maybeBraces, maybeQuotes, maybeDoubleQuotes
, nest, hang, punctuate {- forall -}
, text, int, integer, rational, double, float, char
, empty, {- blankLine, -} semi, comma, colon, space, equals
, lparen, rparen, lbrack, rbrack, lbrace, rbrace
, underscore, dcolon, arrow, darrow, dot, backslash

  -- Set the style
, normal, bold, faint, italic, underline, blink, colnormal, inverse

  -- Set the foreground color
, gray, red, green, yellow, blue, magenta, cyan, white, reset

  -- Set the background color
, bgGray, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite, bgReset

  -- Rendering modes
, renderPlain, renderInColor

  -- The abstract Doc type and the main class
, Doc, PrettyPrint(..), pprPar
) where

import qualified Text.PrettyPrint.HughesPJ as P
import Data.Maybe (isJust)
import Data.List (intersperse)

-- * Colors
-- ----------------------------------------------------------------------------

-- | All colors
data Color = Gray | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
  --          0      1     2       3        4      5         6      7       8
  deriving (Enum)
  -- See [Note] Alternative Strings for Colors

-- | All styles
data Style
  = Normal | Bold | Faint | Italic | Underline | SlowBlink | ColoredNormal | Reverse
  -- 0        1      2       3        4           5           6               7
  deriving (Enum)

-- | Background color to string code (bright)
bgColor :: Color -> String
bgColor c = "\x1b[4" ++ show (fromEnum c) ++ "m"

-- -- | Foreground color to string code (bright)
-- fgColor :: Color -> String
-- fgColor c = "\x1b[9" ++ show (fromEnum c) ++ "m"

-- | Foreground color to string code (dim)
fgColor :: Color -> String
fgColor c = "\27[" ++ show (30 + fromEnum c) ++ "m"

-- | Style to string code
fgStyle :: Style -> String
fgStyle s = "\x1b[" ++ show (fromEnum s) ++ "m"

-- * Pretty-printing modes and the Doc type
-- ----------------------------------------------------------------------------

-- | Pretty printing mode
data PprMode
  = Plain {- No colors -}
  | ColorMode { cm_bg_color :: Color
              , cm_fg_color :: Color
              , cm_tx_style :: Style }

-- | The Doc(ument) type as a reader wrapper around Hughes' and SPJ's Doc type
newtype Doc = D (PprMode -> P.Doc)

-- * Setting colors and styles
-- ----------------------------------------------------------------------------

-- | Special String representato"\27[0m"
modeToString :: PprMode -> String
modeToString mode =  bgColor (cm_bg_color mode)
                  ++ fgColor (cm_fg_color mode)
                  ++ fgStyle (cm_tx_style mode)

-- | Reset mode and set from scratch
resetTo :: PprMode -> String
resetTo mode = "\27[0m" ++ modeToString mode

-- | Change the background color
changeBgColor :: Color -> Doc -> Doc
changeBgColor col (D doc) = D $ \mode -> case mode of
  Plain        -> doc mode
  ColorMode {} ->   P.zeroWidthText (bgColor col)
               P.<> doc (mode {cm_bg_color=col})
               P.<> P.zeroWidthText (resetTo mode) -- reset the previous mode

-- | Change the foreground color
changeFgColor :: Color -> Doc -> Doc
changeFgColor col (D doc) = D $ \mode -> case mode of
  Plain        -> doc mode
  ColorMode {} ->   P.zeroWidthText (fgColor col)
               P.<> doc (mode {cm_fg_color=col})
               P.<> P.zeroWidthText (resetTo mode) -- reset the previous mode

-- | Change the style
changeStyle :: Style -> Doc -> Doc
changeStyle style (D doc) = D $ \mode -> case mode of
  Plain        -> doc mode
  ColorMode {} ->   P.zeroWidthText (fgStyle style)
               P.<> doc (mode {cm_tx_style=style})
               P.<> P.zeroWidthText (resetTo mode) -- reset the previous mode

-- * Lift stuff from Hughes & SPJ library
-- ----------------------------------------------------------------------------

liftDoc :: P.Doc -> Doc
liftDoc d = D (\_ -> d)

liftFun :: (a -> P.Doc) -> (a -> Doc)
liftFun f x = D $ \_ -> f x

liftUnaryOp :: (P.Doc -> P.Doc) -> (Doc -> Doc)
liftUnaryOp op (D d) = D (\m -> op (d m)) -- (op . d)

liftBinaryOp :: (P.Doc -> P.Doc -> P.Doc) -> (Doc -> Doc -> Doc)
liftBinaryOp op (D d1) (D d2) = D (\m -> d1 m `op` d2 m)

liftListOp :: ([P.Doc] -> P.Doc) -> ([Doc] -> Doc)
liftListOp f docs = D (\m -> f [d m | D d <- docs])

-- * Binary combinators
-- ----------------------------------------------------------------------------

-- | Put two documents beside each other
(<>) :: Doc -> Doc -> Doc
(<>) = liftBinaryOp (P.<>)

-- | Put two documents beside each other, separated by a space
(<+>) :: Doc -> Doc -> Doc
(<+>) = liftBinaryOp (P.<+>)

-- | Put a document above another and overlap them if possible
($$) :: Doc -> Doc -> Doc
($$)  = liftBinaryOp (P.$$)

-- | Put a document above another (no overlap)
($+$) :: Doc -> Doc -> Doc
($+$) = liftBinaryOp (P.$+$)

infixr 6 <>, <+>
infixl 5 $$, $+$

-- * List combinators
-- ----------------------------------------------------------------------------

-- | List version of (<>)
hcat :: [Doc] -> Doc
hcat = liftListOp P.hcat

-- | List version of (<+>)
hsep :: [Doc] -> Doc
hsep = liftListOp P.hsep

-- | List version of ($$)
vcat :: [Doc] -> Doc
vcat = liftListOp P.vcat

-- | Either hsep or vcat
sep :: [Doc] -> Doc
sep = liftListOp P.sep

-- | Either hcat or vcat
cat :: [Doc] -> Doc
cat = liftListOp P.cat

-- | "Paragraph fill" version of sep
fsep :: [Doc] -> Doc
fsep = liftListOp P.fsep

-- | "Paragraph fill" version of cat
fcat :: [Doc] -> Doc
fcat = liftListOp P.fcat

-- ----------------------------------------------------------------------------
parens, brackets, braces, quotes, dQuotes :: Doc -> Doc
parens   = liftUnaryOp P.parens
brackets = liftUnaryOp P.brackets
braces   = liftUnaryOp P.braces
quotes   = liftUnaryOp P.quotes
dQuotes  = liftUnaryOp P.doubleQuotes

-- * Wrap it if..
-- ----------------------------------------------------------------------------
maybeParens, maybeBrackets, maybeBraces,
  maybeQuotes, maybeDoubleQuotes :: Bool -> Doc -> Doc
maybeParens       b = if b then parens   else id
maybeBrackets     b = if b then brackets else id
maybeBraces       b = if b then braces   else id
maybeQuotes       b = if b then quotes   else id
maybeDoubleQuotes b = if b then dQuotes  else id

-- * Some usual combinators
-- ----------------------------------------------------------------------------
nest :: Int -> Doc -> Doc
nest i (D d) = D (\m -> P.nest i (d m))

hang :: Doc -> Int -> Doc -> Doc
hang d1 n d2 = vcat [d1, nest n d2]

-- my version: lenngth 2n-1
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _s []     = []
punctuate _s [d]    = [d]
punctuate  s (d:ds) = (d <> s) : punctuate s ds

-- * Transform basic types
-- ----------------------------------------------------------------------------
text :: String -> Doc
text = liftFun P.text

int :: Int -> Doc
int = liftFun P.int

integer :: Integer -> Doc
integer = liftFun P.integer

rational :: Rational -> Doc
rational = liftFun P.rational

double :: Double -> Doc
double = liftFun P.double

float :: Float -> Doc
float = liftFun P.float

char :: Char -> Doc
char = liftFun P.char

-- * Usual separators and other symbols
-- ----------------------------------------------------------------------------
empty :: Doc
empty = liftDoc P.empty

-- || blankLine :: Doc
-- || blankLine = text ""

semi, comma, colon, space, equals :: Doc
semi   = liftDoc P.semi
comma  = liftDoc P.comma
space  = liftDoc P.space
equals = liftDoc P.equals
colon  = liftDoc P.colon

lparen, rparen, lbrack, rbrack, lbrace, rbrace :: Doc
lparen = liftDoc P.lparen
rparen = liftDoc P.rparen
lbrack = liftDoc P.lbrack
rbrack = liftDoc P.rbrack
lbrace = liftDoc P.lbrace
rbrace = liftDoc P.rbrace

underscore, dcolon, arrow, darrow, dot, backslash :: Doc
underscore = liftDoc (P.char '_')
dcolon     = liftDoc (P.text "::")
arrow      = liftDoc (P.text "->")
darrow     = liftDoc (P.text "=>")
dot        = liftDoc (P.char '.')
backslash  = liftDoc (P.char '\\')

-- || forall :: Doc
-- || forall = liftDoc (P.char '\8704')

-- * Rendering Docs
-- ----------------------------------------------------------------------------

-- | The default: No colors
renderPlain :: Doc -> String
renderPlain (D doc) = P.render (doc Plain)

-- | Render with option
renderWithMode :: PprMode -> Doc -> String
renderWithMode mode (D d) = P.render (d mode)

-- | Render in color
renderInColor :: Doc -> String
renderInColor d = renderWithMode defaultPprMode d ++ "\27[0m"

-- | The default mode
defaultPprMode :: PprMode
defaultPprMode = ColorMode { cm_bg_color = Default
                           , cm_fg_color = Default
                           , cm_tx_style = Normal }

-- | Change the style
normal, bold, faint, italic, underline, blink, colnormal, inverse :: Doc -> Doc
normal    = changeStyle Normal
bold      = changeStyle Bold
faint     = changeStyle Faint
italic    = changeStyle Italic
underline = changeStyle Underline
blink     = changeStyle SlowBlink
colnormal = changeStyle ColoredNormal -- TODO: No idea what this is
inverse   = changeStyle Reverse

-- | Change the foreground color
gray, red, green, yellow, blue, magenta, cyan, white, reset :: Doc -> Doc
gray    = changeFgColor Gray
red     = changeFgColor Red
green   = changeFgColor Green
yellow  = changeFgColor Yellow
blue    = changeFgColor Blue
magenta = changeFgColor Magenta
cyan    = changeFgColor Cyan
white   = changeFgColor White
reset   = changeFgColor Default

-- | Change the background color
bgGray, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite, bgReset :: Doc -> Doc
bgGray    = changeBgColor Gray
bgRed     = changeBgColor Red
bgGreen   = changeBgColor Green
bgYellow  = changeBgColor Yellow
bgBlue    = changeBgColor Blue
bgMagenta = changeBgColor Magenta
bgCyan    = changeBgColor Cyan
bgWhite   = changeBgColor White
bgReset   = changeBgColor Default

-- * The PrettyPrint class
-- ----------------------------------------------------------------------------
class PrettyPrint a where
  ppr :: a -> Doc
  needsParens :: a -> Bool

pprPar :: PrettyPrint a => a -> Doc
pprPar x | needsParens x = parens (ppr x)
         | otherwise     = ppr x

instance PrettyPrint Int where
  ppr           = int
  needsParens _ = False

instance PrettyPrint Char where
  ppr           = char
  needsParens _ = False

instance PrettyPrint Float where
  ppr           = float
  needsParens _ = False

instance PrettyPrint Double where
  ppr           = double
  needsParens _ = False

instance PrettyPrint Bool where
  ppr True      = text "True"
  ppr False     = text "False"
  needsParens _ = False

instance PrettyPrint a => PrettyPrint [a] where
  ppr           = brackets . fsep . punctuate comma . map ppr
  needsParens _ = False

instance PrettyPrint () where
  ppr ()        = text "()"
  needsParens _ = False

instance PrettyPrint a => PrettyPrint (Maybe a) where
  ppr Nothing  = text "Nothing"
  ppr (Just x) = text "Just" <+> pprPar x
  needsParens  = isJust

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  ppr (Left  x) = text "Left"  <+> pprPar x
  ppr (Right y) = text "Right" <+> pprPar y
  needsParens _ = True

-- NOTE: No spaces, no newlines. All together
pprTuple :: [Doc] -> Doc
pprTuple = parens . hcat . intersperse comma

-- || BETTER FOR TAKING WIDTH INTO ACCOUNT:
-- ||     -- | Pretty-print a list of Outputables as a tuple
-- ||     -- (comma-separated and wrapped with parentheses)
-- ||     pprTuple :: Outputable a => [a] -> Doc
-- ||     pprTuple = parens . fsep . punctuate comma . map ppr

-- | PrettyPrint tuples of size 2
instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a,b) where
  ppr (x,y)     = pprTuple [ppr x, ppr y]
  needsParens _ = False

-- | PrettyPrint tuples of size 3
instance (PrettyPrint a, PrettyPrint b, PrettyPrint c) => PrettyPrint (a,b,c) where
  ppr (x,y,z)   = pprTuple [ppr x, ppr y, ppr z]
  needsParens _ = False

-- | PrettyPrint tuples of size 4
instance (PrettyPrint a, PrettyPrint b, PrettyPrint c, PrettyPrint d)
    => PrettyPrint (a,b,c,d) where
  ppr (x,y,z,w) = pprTuple [ppr x, ppr y, ppr z, ppr w]
  needsParens _ = False

-- | PrettyPrint tuples of size 5
instance (PrettyPrint a, PrettyPrint b, PrettyPrint c, PrettyPrint d, PrettyPrint e)
    => PrettyPrint (a,b,c,d,e) where
  ppr (x,y,z,w,r) = pprTuple [ppr x, ppr y, ppr z, ppr w, ppr r]
  needsParens _   = False

-- | PrettyPrint tuples of size 6
instance ( PrettyPrint a, PrettyPrint b, PrettyPrint c
         , PrettyPrint d, PrettyPrint e, PrettyPrint f )
    => PrettyPrint (a,b,c,d,e,f) where
  ppr (x,y,z,w,r,s) = pprTuple [ppr x, ppr y, ppr z, ppr w, ppr r, ppr s]
  needsParens _     = False

-- | PrettyPrint tuples of size 7
instance ( PrettyPrint a, PrettyPrint b, PrettyPrint c
         , PrettyPrint d, PrettyPrint e, PrettyPrint f, PrettyPrint g )
    => PrettyPrint (a,b,c,d,e,f,g) where
  ppr (x,y,z,w,r,s,t) = pprTuple [ppr x, ppr y, ppr z, ppr w, ppr r, ppr s, ppr t]
  needsParens _       = False

-- | PrettyPrint tuples of size 8
instance ( PrettyPrint a, PrettyPrint b, PrettyPrint c, PrettyPrint d
         , PrettyPrint e, PrettyPrint f, PrettyPrint g, PrettyPrint h )
    => PrettyPrint (a,b,c,d,e,f,g,h) where
  ppr (x,y,z,w,r,s,t,u) = pprTuple [ppr x, ppr y, ppr z, ppr w, ppr r, ppr s, ppr t, ppr u]
  needsParens _         = False




-- | Check whether the current terminal supports colors and font changes
-- ----------------------------------------------------------------------------

-- || -- | Whether or not the current terminal supports pretty-terminal
-- || supportsPretty :: IO Bool
-- || supportsPretty =
-- ||   hSupportsANSI stdout
-- ||   where
-- ||     -- | Use heuristics to determine whether the functions defined in this
-- ||     -- package will work with a given handle.
-- ||     --
-- ||     -- The current implementation checks that the handle is a terminal, and
-- ||     -- that the @TERM@ environment variable doesn't say @dumb@ (whcih is what
-- ||     -- Emacs sets for its own terminal).
-- ||     hSupportsANSI :: Handle -> IO Bool
-- ||     -- Borrowed from an HSpec patch by Simon Hengel
-- ||     -- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
-- ||     hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
-- ||       where
-- ||         isDumb = (== Just "dumb") <$> lookupEnv "TERM"

{- [Note] Alternative Strings for Colors

  Some alternative strings for the same colors and styles (but missing some).
  They seem to be dimmer than the ones that this library provides.

      reset     = "\27[0m"
      bold      = "\27[;1m"
      gray      = "\27[30m"
      red       = "\27[31m"
      green     = "\27[32m"
      yellow    = "\27[33m"
      blue      = "\27[34m"
      purple    = "\27[35m"
      turquoise = "\27[36m"
      white     = "\27[37m"
-}
