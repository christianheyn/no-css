module NoMoreCss.Main where

    import NoMoreCss.Selectors
    import NoMoreCss.CSS (
                        padding,
                        margin,
                        widthHeight,
                        font,
                        position,
                        display)
    import NoMoreCss.Docs
    import Data.List

    main = do
        let allSelectors = concat [
                  padding
                , margin
                , widthHeight
                , font
                , position
                , display
                ]
        let content = selectorsToString allSelectors
        writeFile "styles/no-more-css.css"        content
        writeFile "styles/no-more-css.module.css" content
        writeFile "styles/no-more-css.scss"       content
        writeFile "styles/no-more-css.less"       content
        writeFile "docs/no-more-css.css"          content
        writeFile "docs/docu.html" (docuBody $ concat $ map toDocuHtmlDiv allSelectors)
        -- print $ concat $ map toDocuHtmlDiv display
