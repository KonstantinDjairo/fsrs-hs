{-# LANGUAGE TemplateHaskell #-}

module SpacedRepetition.TH (makeNewtype) where

import Language.Haskell.TH

-- Generate newtypes programmatically
makeNewtype :: String -> Q [Dec]
makeNewtype name = do
    let tyName = mkName name
    return
        [ NewtypeD
            []                       -- No context
            tyName                   -- Name of the type
            []                       -- No type parameters
            Nothing                  -- No kind signature
            (NormalC tyName [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Double)])
            [ DerivClause Nothing    -- Deriving clause
                [ ConT ''Show
                , ConT ''Eq
                , ConT ''Ord
                , ConT ''Num
                , ConT ''Fractional
                , ConT ''Floating
                ]
            ]
        ]
