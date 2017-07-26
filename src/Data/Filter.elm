module Data.Filter exposing (Filter(All, CreatedBy, AssignedTo, HasMentionOf))


type Filter
    = All
    | CreatedBy String
    | AssignedTo String
    | HasMentionOf String
