type size = int
type hash = int
type segment = hash * hash * size
type choice = Left | Right
type player = address

type storage = unit

type refutation_parameter =
    | Choose of choice
    | Split of segment * segment
    | Start of segment * player * player