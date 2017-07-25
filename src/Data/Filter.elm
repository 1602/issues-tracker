module Data.Filter exposing (Filter(..))


type Filter
    = All
    | CreatedBy String
    | AssignedTo String
    | HasMentionOf String
