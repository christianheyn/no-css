module NoMoreCss.Docs where

    import NoMoreCss.Selectors
    import NoMoreCss.CSS (
                            padding,
                            margin,
                            widthHeight,
                            font,
                            position,
                            display)


    -- data Selector = Selector {
    --     selector:: String,
    --     props:: [(String, String)]
    -- } deriving (Eq)

    toDocuHtmlDiv :: Selector -> String
    toDocuHtmlDiv s = concat [
          "<div class=\"p15px\" title=\"" ++ css ++ "\" id=\"" ++ sel ++ "\">"
        , "<a href=\"#" ++ sel ++ "\" class=\"di pr15px\">@"
        , "</a>"
        , "<input readonly class=\"di p5px fs20px\" value=\"" ++ sel ++ "\" />"
        , "<pre class=\"di ml20px fs14px\"><code>" ++ css ++ "</code></pre>"
        , "</div>"
        ]
        where sel = selector s
              css = show s

    docuBody content = concat [
          "<html>"
        , "<head>"
        , "<link rel=\"stylesheet\" href=\"./no-more-css.css\" />"
        , "</head>"
        , "<body>"
        , content
        , "</body>"
        ]
