# observe

Observe is a do-notation-based DSL library for discrete Bayesian decision theory using the subdistribution monad.
The following code solves the student network example from "Probabilistic Machine Learning: Advanced Topics" by Murphy.

```haskell
student :: Distribution Intelligence
student = do
  diff <- distribution [(Easy, 6/10), (Hard, 4/10)]
  intel <- distribution [(Low, 7/10), (High, 3/10)]
  grade <- case (intel, diff) of
    (Low,  Easy) -> distribution [(A, 30/100), (B, 40/100), (C, 30/100)]
    (Low,  Hard) -> distribution [(A,  5/100), (B, 25/100), (C, 70/100)]
    (High, Easy) -> distribution [(A, 90/100), (B,  8/100), (C,  2/100)]
    (High, Hard) -> distribution [(A, 50/100), (B, 30/100), (C, 20/100)]
  sat <- case intel of
    Low  -> distribution [(Good,  5/100), (Bad, 95/100)]
    High -> distribution [(Good, 80/100), (Bad, 20/100)]
  letter <- case grade of
    A -> distribution [(Positive, 10/100), (Negative, 90/100)]
    B -> distribution [(Positive, 40/100), (Negative, 60/100)]
    C -> distribution [(Positive, 99/100), (Negative,  1/100)]
  observe (grade == C)
  observe (sat == Good)
  return intel
```

The following code solves the Monty Hall problem.

```haskell
montyHall :: Distribution Door
montyHall = do
  car <- uniform [Left,Middle,Right]
  choice <- uniform [Left,Middle,Right]
  announcement <- host car choice
  observe (choice == Middle)
  observe (announcement == Left)
  return car
```  