Here, I outline overview of our chase for the problem that the basic pedlr model shifts mean estimate in the same direction.

---

### Problem Definition
While it was anticipated that increased learning from large prediction errors will shift means towards their tails, this does not occur when a decision rule (softmax/greedy) is introduced. Both effects are to the left of the mean (or towards less sampled cues).
![problem_description](img/problem_def.png)
### Clues
#### Clue 1: Force choice fixes the problem

#### Clue 2: Number of times a cue was chosen correlates with error from mean
