NB. USAGE EXAMPLE WITH `ex` MDP (defined below)
NB. `mdp ex`
NB. ==> 41.4286 35.7143 0

NB. `ex`: list of actions per state.
NB. - running example in explanations of this module's functions (verbs/adverbs/conjunctions).
NB. - expected structure: (actions of first state) , (actions of second state) ,: (actions of final state - empty list -)
ex =: ((10 0.5 0.3 0.2) ; (25 0.2 0.7 0.1)) , ((10 0 0.75 0.25) ; (25 0 0.3 0.7)) ,: 0 $ 0

NB. `mdp`: fixed point iteration over bellman equations with initial value
mdp =: b_iter ^:_ v_init

NB. `v_init`: returns the initial V(state) for each state (i.e: an array filled with as many 0s as states).
v_init =: #&0 @: # @: >

NB. `b_iter`: is a dyadic verb (function) where:
NB. right parameter: set of values V(state) for each state
NB. left parameter: list of actions per state as in `ex`
NB. returns a list with the same shape as its left parameter, where each item is the optimal cost of a state (minimum).
b_iter =: dyad define
NB. `a`: rank-3 matrix where each item (2-cell) corresponds to a state, and each 1-cell corresponds to actions from that state
NB. `c`: rank-2 matrix where each item (1-cell) corresponds to a state, and each 0-cell corresponds to the base cost of an action from that state
NB. `t`: rank-3 matrix where each item (2-cell) corresponds to a state, and each 1-cell corresponds to probabilities of going to another state from that state
a =. >x
c =. {."1 a
t =. }."1 a
<./"1 c + +/"1 t *"1 y
)