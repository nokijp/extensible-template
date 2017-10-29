# Extensible Template

Extensible Template is a template engine that enables embedding functions into a text document.
Functions can be defined by users using `Extension`.


## Converting Templates

```haskell
extension1 :: ExtensionId (State Int)
extension1 = ...

extension2 :: ExtensionId Identity
extension2 = ...

main :: IO ()
main = do
  let extensions = extension2 <|< extension1 <|< ENil
  template <- readFile "template"
  let result = runIdentity $ runTemplate extensions template
  
  either (hPutStrLn stderr) putStrLn result
```


## Defining Extensions

The following program is an example for defining an extension that returns a constant string specified in `main`.

```haskell
constExtension :: String -> ExtensionId Identity
constExtension value = Extension
  { extensionAcceptor = (== "const")
  , extensionFunction = const $ const $ return $ Right value
  , extensionRunner = return . runIdentity
  }

main :: IO ()
main = do
  let extensions = constExtension "abc" <|< ENil
  let template = "{ const }"
  let result = runIdentity $ runTemplate extensions template

  either (hPutStrLn stderr) putStrLn result  -- "abc"
```

The following program is an example for defining an extension that sum up arguments.

```haskell
sumExtension :: ExtensionId Identity
sumExtension = Extension
  { extensionAcceptor = (== "sum")
  , extensionFunction = const f
  , extensionRunner = return . runIdentity
  } where
    f :: [Param] -> Identity (Either String String)
    f ps = return $ show <$> sumParams ps
    sumParams :: [Param] -> Either String Integer
    sumParams [] = Right 0
    sumParams (IntParam n : ps) = (+ n) <$> sumParams ps
    sumParams _ = Left "\"sum\" only accepts integer parameters"

main :: IO ()
main = do
  let extensions = sumExtension <|< ENil
  let template = "{ sum 1 2 3 4 5 }"
  let result = runIdentity $ runTemplate extensions template

  either (hPutStrLn stderr) putStrLn result  -- "15"
```

More examples are available in [BasicExtensions.hs](src/ExtensibleTemplate/BasicExtensions.hs).
