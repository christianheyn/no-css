module NoMoreCss.CSS (
    padding,
    margin,
    widthHeight,
    font,
    position,
    display) where

    import NoMoreCss.Selectors
    import Data.List

    pixelNs = (sort $ take 67 ([0,1..19 ] ++ [20, 25..]))
    percentNs = [0, 1..100]

    withPxConfig :: String -> String -> [Selector]
    withPxConfig className propName = createSelectors $ Config {
           _unitName  = "px",
           _classNamePrefix = className,
           _classNameSuffix = "px",
           _propName  = propName,
           _numberTransformer = show,
           _ns = pixelNs
        }

    withPercentConfig :: String -> String -> [Selector]
    withPercentConfig className propName = createSelectors $ Config {
           _unitName  = "%",
           _classNamePrefix = className,
           _classNameSuffix = "",
           _propName  = propName,
           _numberTransformer = show,
           _ns = percentNs
        }

    decimal :: Int -> Float
    decimal n = (read s) / 10
        where s = show n

    withRemConfig :: String -> String -> [Selector]
    withRemConfig className propName = createSelectors $ Config {
           _unitName  = "rem",
           _classNamePrefix = className,
           _classNameSuffix = "rem",
           _propName  = propName,
           _numberTransformer = (show . decimal),
           _ns = pixelNs
        }

    -- PADDING ============================================================================

    px n = Selector {
            selector = "px" ++ show n,
            props = [("padding-left", show n ++ "px"), ("padding-right", show n ++ "px")]
            }

    py n = Selector {
            selector = "py" ++ show n,
            props = [("padding-top", show n ++ "px"), ("padding-bottom", show n ++ "px")]
            }

    padding = concat [
          withPxConfig  "pt" "padding-top"
        , withRemConfig "pt" "padding-top"
        , withPxConfig  "pr" "padding-right"
        , withRemConfig "pr" "padding-right"
        , withPxConfig  "pb" "padding-bottom"
        , withRemConfig "pb" "padding-bottom"
        , withPxConfig  "pl" "padding-left"
        , withRemConfig "pl" "padding-left"
        , withPxConfig  "p"  "padding"
        , withRemConfig "p"  "padding"
        , map px pixelNs
        , map py pixelNs
        , [
              Selector { selector = "pta", props = [("padding-top", "auto")] }
            , Selector { selector = "pra", props = [("padding-right", "auto")] }
            , Selector { selector = "pba", props = [("padding-bottom", "auto")] }
            , Selector { selector = "pla", props = [("padding-left", "auto")] }
            , Selector { selector = "pxa", props = [("padding-left", "auto"), ("padding-right", "auto")] }
            , Selector { selector = "pya", props = [("padding-top", "auto"), ("padding-bottom", "auto")] }
            , Selector { selector = "pa", props = [("padding", "auto")] }
          ]
        ]

    -- MARGIN ============================================================================

    mx n = Selector {
            selector = "mx" ++ show n,
            props = [("margin-left", show n ++ "px"), ("margin-right", show n ++ "px")]
            }

    my n = Selector {
            selector = "my" ++ show n,
            props = [("margin-top", show n ++ "px"), ("margin-bottom", show n ++ "px")]
            }

    margin = concat [
          withPxConfig  "mt" "margin-top"
        , withRemConfig "mt" "margin-top"
        , withPxConfig  "mr" "margin-right"
        , withRemConfig "mr" "margin-right"
        , withPxConfig  "mb" "margin-bottom"
        , withRemConfig "mb" "margin-bottom"
        , withPxConfig  "ml" "margin-left"
        , withRemConfig "ml" "margin-left"
        , withPxConfig  "m"  "margin"
        , withRemConfig "m"  "margin"
        , map mx pixelNs
        , map my pixelNs
        , [
              Selector { selector = "mta", props = [("margin-top", "auto")] }
            , Selector { selector = "mra", props = [("margin-right", "auto")] }
            , Selector { selector = "mba", props = [("margin-bottom", "auto")] }
            , Selector { selector = "mla", props = [("margin-left", "auto")] }
            , Selector { selector = "mxa", props = [("margin-left", "auto"), ("margin-right", "auto")] }
            , Selector { selector = "mya", props = [("margin-top", "auto"), ("margin-bottom", "auto")] }
            , Selector { selector = "ma", props = [("margin", "auto")] }
          ]
        ]

    -- WIDTH HEIGHT MIN MAX ===============================================================

    widthHeight = concat [
          withPercentConfig "w" "width"
        , withPercentConfig "minw" "min-width"
        , withPercentConfig "maxw" "max-width"
        , withPercentConfig "h" "height"
        , withPercentConfig "minh" "min-height"
        , withPercentConfig "maxh" "max-height"
        , [
              Selector { selector = "wa", props = [("width", "auto")] }
            , Selector { selector = "minwa", props = [("min-width", "auto")] }
            , Selector { selector = "maxwa", props = [("max-width", "auto")] }
          ]
        , [
              Selector { selector = "ha", props = [("height", "auto")] }
            , Selector { selector = "minha", props = [("min-height", "auto")] }
            , Selector { selector = "maxha", props = [("max-height", "auto")] }
          ]
        ]

    -- FONT ==============================================================================

    withFontPxConfig :: String -> String -> [Selector]
    withFontPxConfig className propName = createSelectors $ Config {
           _unitName  = "px",
           _classNamePrefix = className,
           _classNameSuffix = "px",
           _propName  = propName,
           _numberTransformer = show,
           _ns = [0, 1..100]
        }

    withFontRemConfig :: String -> String -> [Selector]
    withFontRemConfig className propName = createSelectors $ Config {
           _unitName  = "rem",
           _classNamePrefix = className,
           _classNameSuffix = "rem",
           _propName  = propName,
           _numberTransformer = (show . decimal),
           _ns = [0, 1..100]
        }

    font = concat [
          withFontPxConfig  "lh" "line-height"
        , withFontRemConfig "lh" "line-height"
        , withFontPxConfig  "fs" "font-size"
        , withFontRemConfig "fs" "font-size"
        ]

    position = [
          Selector { selector = "posabs", props = [("position", "absolute")] }
        , Selector { selector = "posfix", props = [("position", "fixed")] }
        , Selector { selector = "possti", props = [("position", "sticky")] }
        , Selector { selector = "posrel", props = [("position", "relative")] }
        ]

    display = [
          Selector { selector = "db", props = [("display", "block")] }
        , Selector { selector = "dib", props = [("display", "inline-block")] }
        , Selector { selector = "di", props = [("display", "inline")] }
        , Selector { selector = "dfx", props = [("display", "flex")] }
        ]
