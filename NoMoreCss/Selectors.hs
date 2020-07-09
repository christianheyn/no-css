module NoMoreCss.Selectors where

    data Selector = Selector {
        selector:: String,
        props:: [(String, String)]
    } deriving (Eq)

    instance Show Selector where
        show x = ("." ++ filter (/= '.') (selector x))
            ++ " { "
            ++ (concat $ map propToString (props x))
            ++ "}\n"
            where propToString (k, v) = k ++ ": " ++ v ++ "; "

    data Config = Config {
          _unitName          :: String
        , _classNamePrefix   :: String
        , _classNameSuffix   :: String
        , _propName          :: String
        , _numberTransformer :: (Int -> String)
        , _ns                :: [Int]
        }

    selectorsToString :: [Selector] -> String
    selectorsToString xs = concat $ map show xs

    createSelectors :: Config -> [Selector]
    createSelectors config = map go (_ns config)
        where go n = Selector {
                selector = pre ++ (show n) ++ suf,
                props = [((_propName config), (transform n) ++ (_unitName config))]
                }
              transform = _numberTransformer config
              pre = _classNamePrefix config
              suf = _classNameSuffix config