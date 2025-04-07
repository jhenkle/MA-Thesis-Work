module Language where

-- Objects
data LanguageObj
    = Subject -- $\Phi x$
    | Subject' -- $\Phi x^{\prime}$
    | FieldOfOther -- A
    deriving(Show,Eq)

-- Morphisms
data LanguageMorph
    = IdM LanguageObj -- Identity Morphisms
    | Eq LanguageObj LanguageObj -- Equality Relation
    | Language LanguageObj LanguageObj -- $\Phi x \to A$
    | Metonymy LanguageObj LanguageObj -- $\Phi x \to \Phi x^{\prime}$
    | Metaphor LanguageObj LanguageObj -- $A \to \Phi x^{\prime}$
    deriving (Show, Eq)
-- Composition of morphisms
compose :: LanguageMorph -> LanguageMorph -> Maybe LanguageMorph
compose (Language a b) (Metaphor b' c)
    | b == b'   = Just (Metonymy a c)
    | otherwise = Nothing
compose _ _ = Nothing

verifyCommutativity :: Bool
verifyCommutativity =
    let j = Language Subject FieldOfOther
        f = Metonymy Subject Subject'
        af' = Metaphor FieldOfOther Subject'
        idX = IdM Subject
    in compose j af' == Just f