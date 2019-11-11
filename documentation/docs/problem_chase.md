Here, I outline overview of our chase for the problem that the basic pedlr model shifts mean estimate in the same direction.

---

### Problem Definition
While it was anticipated that increased learning from large prediction errors will shift means towards their tails, this does not occur when a decision rule (softmax/greedy) is introduced. Both effects are to the left of the mean (or towards less sampled cues).
![problem_description](img/problem_def.png)
### Clues
  1. **Clue 1**: Force choice fixes the problem
    - Sampling all cues the same amount of times makes the bias in the predicted way
  2. **Clue 2**: Number of times a cue was chosen correlates with error from mean
    - The more a cue was chosen the lower the deviation from the true mean
      - Cue 1 r=0.49522 p=0.00025586
      - Cue 2 r=0.64743 p=3.7483e-07
      - Cue 3 r=0.47346 p=0.00051522
    - this seems also related to uncertainty:
  3. **Clue 3**: The less a cue is sampled the bigger the error
    - similar to clue 2, but now we are looking at between cue sampling
    - if there are 3 cues with fixed variance the least sampled one will have largest bias from the mean
  4. **Clue 4**: The same effect across cues occurs irrespective of the valence
    - it seems that it's the relative number of choices and the valence doesn't matter
