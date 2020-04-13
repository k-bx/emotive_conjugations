module Le.Spacy exposing (..)

import Html


{-| <https://spacy.io/api/annotation#dependency-parsing>
-}
depExplanation : String -> String
depExplanation x =
    case x of
        "acl" ->
            "clausal modifier of noun (adjectival clause)"

        "acomp" ->
            "adjectival complement"

        "advcl" ->
            "adverbial clause modifier"

        "advmod" ->
            "adverbial modifier"

        "agent" ->
            "agent"

        "amod" ->
            "adjectival modifier"

        "appos" ->
            "appositional modifier"

        "attr" ->
            "attribute"

        "aux" ->
            "auxiliary"

        "auxpass" ->
            "auxiliary (passive)"

        "case" ->
            "case marking"

        "cc" ->
            "coordinating conjunction"

        "ccomp" ->
            "clausal complement"

        "compound" ->
            "compound"

        "conj" ->
            "conjunct"

        "cop" ->
            "copula"

        "csubj" ->
            "clausal subject"

        "csubjpass" ->
            "clausal subject (passive)"

        "dative" ->
            "dative"

        "dep" ->
            "unclassified dependent"

        "det" ->
            "determiner"

        "dobj" ->
            "direct object"

        "expl" ->
            "expletive"

        "intj" ->
            "interjection"

        "mark" ->
            "marker"

        "meta" ->
            "meta modifier"

        "neg" ->
            "negation modifier"

        "nn" ->
            "noun compound modifier"

        "nounmod" ->
            "modifier of nominal"

        "npmod" ->
            "noun phrase as adverbial modifier"

        "nsubj" ->
            "nominal subject"

        "nsubjpass" ->
            "nominal subject (passive)"

        "nummod" ->
            "numeric modifier"

        "oprd" ->
            "object predicate"

        "obj" ->
            "object"

        "obl" ->
            "oblique nominal"

        "parataxis" ->
            "parataxis"

        "pcomp" ->
            "complement of preposition"

        "pobj" ->
            "object of preposition"

        "poss" ->
            "possession modifier"

        "preconj" ->
            "pre-correlative conjunction"

        "prep" ->
            "prepositional modifier"

        "prt" ->
            "particle"

        "punct" ->
            "punctuation"

        "quantmod" ->
            "modifier of quantifier"

        "relcl" ->
            "relative clause modifier"

        "root" ->
            "root"

        "xcomp" ->
            "open clausal complement"

        "ROOT" ->
            "root element. no dependency"

        _ ->
            "unknown. please see: https://spacy.io/api/annotation#dependency-parsing"
