
To use this code:

  You'll need haskell and parsec
   $ apt-get install ghc6 cabal-install 
   $ cabal install parsec

Then:

   $ ./build.sh      # to build
   $ ./impl1         # to run


based on
http://www.ietf.org/id/draft-levine-dnsextlang-00.txt

In this code, QUESTION is a question/bug for the spec and BUG is a
bug that I know exists in the way my code implements the spec.

> import Data.Char
> import Data.Either
> import Numeric
> import Text.Parsec
> import Text.Parsec.String

> main = do
>   putStrLn "dnsextlang test 1, copyright 2011 CQX Limited"
>   defs <- parseFromFile parseMungedLines "rrtypes"
>   let (Right l) = defs -- assume error free parsing
>   putStrLn "Munged lines from rrtypes:"
>   print l

At this stage of execution, each line of l is a string that should contain
a complete RRTYPE definition (assuming the input file is in the correct
format)

Now we'll process each line individually. I want to end up with a parser
that will recognise partial RR lines and output the appropriate RRTYPE
record, or skip (so that I can try all parsers on an example line)

>   let rrparsers = map stringToRRParser l
>   putStrLn "parser results:"
>   print rrparsers
>   putStrLn "describe results:"
>   print $ map describeRRParser rrparsers
>   putStrLn "test against example MX"
>   processMaster rrparsers
>   putStrLn "The end."

==== the following section deals with parsing the rrtypes file into lines.

| Each line of the extension language file is a comment, if it is blank
| or it starts with a # character, or a definition of an RRTYPE.

not true... it can be a continuation too.

QUESTION: Do continuation lines apply to comments?
ASSUMPTION: no.

QUESTION: How does:
   foo \
   # bar
   baz
get interpreted?  as foo baz  or as foo #bar <new definition> baz?

Lets pick a fairly arbitrary form for now.

Here, I'll parse everything into lines, then munge things to strip
comments and join continuations:

> parseLines = (many1 (noneOf "\n")) `sepEndBy` newline

> parseMungedLines = do
>    rawLines <- parseLines
>    let linesWithoutComments = filter
>                                 (\s -> head s /= '#')
>                                 rawLines
>    let joinedLines = joinSlashedLines linesWithoutComments
>    return joinedLines

POSSIBLE BUG IN MY CODE: I don't strip off leading whitespace on
  the continuation line. But perhaps later parser wont' care about that?

QUESTION: How are tokens separated? by 'many spaces' or a single space?

> joinSlashedLines :: [String] -> [String]
> joinSlashedLines lines = foldr f [] lines where
>    f line acclines =
>        if last line == '\\' then ((init line) ++ head acclines) : (tail acclines)
>           else line : acclines


==== the following converts an individual line into a parser that will
     recognise the corresponding zone master file format

> stringToRRParser :: String -> RRType
> stringToRRParser s = let
>    p = parse rrparser "rrtypes file" s
>   in case p of
>     Right r -> r
>     Left err -> error ("Invalid RRType definition: " ++ (show err))


| Each line starts with the a token containing the name of the RRTYPE,
|   a colon, and the numeric RRTYPE. 

> rrparser = do
>   tn <- parseRRtypename
>   char ':'
>   n <- parseRRnumber
>   spaces
>   t <- parseRRtoken `sepEndBy` spaces
>   return $ RRType {rrname = tn, rrnumber = (read n), rrtokens = t}

> parseRRtypename = many1 alphaNum -- maybe need more restrictions here, from DNS specs

> parseRRnumber = many1 digit


> parseRRtoken = do
>   tokenName <- many1 alphaNum
>   case tokenName of
>     "I1" -> parseTokenI1
>     "I2" -> parseTokenI2
>     "I4" -> parseTokenI4
>     "N" -> parseTokenN
>     "A" -> parseTokenA
>     _ -> fail $ "UNIMPLEMENTED TOKEN " ++ tokenName


BUG: these I<n> need to take some parameters. but the MX example doesn't
     use them so I haven't implemented them yet


> parseTokenI1 = parseTokenI (I1)
> parseTokenI2 = parseTokenI (I2)
> parseTokenI4 = parseTokenI (I4)

> parseTokenI constr = do
>   r <- optionMaybe $ do
>     oneOf "["
>     let pair = do
>          sym <- many1 (noneOf ",=|<>\\")
>          oneOf "="
>          value <- many1 digit
>          return (sym, (read value) :: Integer )
>     l <- pair `sepBy` oneOf ","
>     oneOf "]"
>     return l
>   return $ constr r


> parseTokenN = do
>   -- take paramers here BUG
>   -- for now, just skip them
>   many (noneOf " ")
>   return N

> parseTokenA = do
>   -- no parameters allowed
>   return A


   return $ tokenName ++ " with parameters " ++ (show rest)
   rest <- many (noneOf " ")


==== This section defines the parse tree as haskell data structures

> data RRType = RRType {
>   rrname :: String,
>   rrnumber :: Integer,
>   rrtokens :: [RRToken]
>   } deriving Show

> data RRToken = A
>              | I1 (Maybe [(String, Integer)])
>              | I2 (Maybe [(String, Integer)])
>              | I4 (Maybe [(String, Integer)])
>              | N
>              deriving Show



==== now here is some code to describe RRTypes in english

> describeRRParser :: RRType -> String
> describeRRParser rrtype = "A(n) " ++ (rrname rrtype) ++ " has type code " ++ (show $ rrnumber rrtype)


==== and some code to convert RRs into RFC3597 s5 representation

Given an RR type and a string that is (eg) read from master zone file,
output a replacement line for the master zone file in RFC3597 format

> convertToRFC3597 rrtype record = let
>       showOctet o = if o >= 16 then showHex o " " else "0" ++ showHex o " "
>       result = octetsForRFC3597 rrtype record
>    in case result of
>      Right octets ->
>       Right ("TYPE" ++ (show $ rrnumber rrtype) ++ " \\# " ++
>       (show $ length octets) ++ " " ++ (foldr1 (++) (map showOctet octets)))
>      Left l -> Left l

BUG/QUESTION: Do I need to write out octets

Now we need to convert our RR parsers into parsec parsers, and use
those to create an octet stream.

> octetsForRFC3597 rrtype string =  let
>    parser = parserForRFC3597 rrtype
>  in parse parser "master file syntax line" string

> parserForRFC3597 :: RRType -> Parser [Integer]
> parserForRFC3597 rrtype = do
>   string (rrname rrtype)
>   spaces
>   s <- mapM (parserForToken) (rrtokens rrtype)
>   return (foldr1 (++) s)

> parserForToken (I1 params) = parserForI_n 1 params
> parserForToken (I2 params) = parserForI_n 2 params
> parserForToken (I4 params) = parserForI_n 4 params

> parserForToken N = do
>   labels <- (many1 alphaNum) `sepEndBy` (oneOf ".")
>   -- o is [String]
>   let labelToOcts s = map fromIntegral ([length s] ++ (map ord s)) :: [Integer]
>   let labelocts = (map labelToOcts labels) ++ [[0]] :: [[Integer]]
>   spaces
>   return $ foldr1 (++) labelocts

BUG: probably don't encode labels properly - for example, how is the
missing/non-missing final dot dealt with? Do I need to know the origin of
the zone to do this properly?

> parserForToken A = do
>   a <- many1 alphaNum
>   oneOf "."
>   b <- many1 alphaNum
>   oneOf "."
>   c <- many1 alphaNum
>   oneOf "."
>   d <- many1 alphaNum
>   return ([read a, read b, read c, read d] :: [Integer])

BUG: are there other ways to write IP addresses?

When parsing an I, we might be given an integer, or we might be given
a token.

QUESTION: It looks like using symbolic form excludes the use of numbers in
a field. Is that the case? or are mixed symbolic and numeric forms allowed?

> parserForI_n octs params = (maybe parserForI_nAsDigits parserForI_nAsSymbols params) octs

> parserForI_nAsDigits octs = do
>   n <- many1 digit
>   spaces
>   let unpadded = toBase 256 ( read n :: Integer)
>   let padding = take ((octs - length unpadded) `max` 0) (repeat 0)
>   return $ padding ++ unpadded

> parserForI_nAsSymbols params octs = do
>   let parserForSymbol (s,n) = try $ do
>        string s
>        spaces
>        return n
>   let parsers = map parserForSymbol params
>   n <- choice parsers
>   let unpadded = toBase 256 n
>   let padding = take ((octs - length unpadded) `max` 0) (repeat 0)
>   return $ padding ++ unpadded



==== Now we can process the example master file

> processMaster p = do
>  putStrLn "Processing zone.master"
>  mastercontent <- readFile "zone.master"
>  let ls = lines mastercontent
>  mapM_ (processMasterLine p) ls

> processMasterLine parsers l = do
>  putStrLn $ "; " ++ l
>  let f parser = convertToRFC3597 parser l
>  let strs = map f parsers
>  let rs = rights strs
>  if length rs > 0 then
>    mapM_ putStrLn rs
>   else putStrLn "; no parse for this type"


==== some helper functions:

base conversion:

> toBase base n = let
>   lsd = n `rem` base
>   rest = n `quot` base
>  in if rest == 0 then [lsd]
>      else  (toBase base rest) ++ [lsd]

