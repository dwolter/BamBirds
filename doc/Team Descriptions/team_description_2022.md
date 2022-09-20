# BamBirds 2022 Agent Description

The BamBird agent is developed in context of student projects at the University of Bamberg, Germany. By design, BamBird integrates GOFAI (Good Old-Fashioned AI) approaches like symbolic state space representation with probabilistic methods and machine learning. An explicit qualitative representation of levels is central to the design of the agent. BamBirds comprise the following building blocks, listed in order of a typical data processing cycle.

The first component, mainly using parts provided by the naive agent framework, is responsible for interpreting screen shots and interacting with the game. We generate a declarative description of the screen content, enriched by qualitative relations between individual objects and structures detected. This component will also monitor the actions performed and use information gathered when observing how the game reacts in an online learning manner to improve precision of the description generated.

The second and most involved component is responsible for determining possible shot candidates, given a scene description. By obtaining explicit representation of qualitative relationships such as, for example, above(pig,ice) it is possible to design rules that serve as heuristics to identify useful shots. One of these rules states that by destroying the single object that supports another, the now unsupported object will fall down an be destroyed. In our example, aiming at the ice object could thus be a viable plan. While BamBirds does not perform physical simulation, the symbolic method is augmented with a quantitative estimator, e.g., to estimate the likelihood of penetrating objects by a single shot. Computing shot candidates is performed as simple partial order planning. A prediction of which targets are destroyed, affected or freed (obstacles between the target and the bird removed) as well as a probability of the effects happening (uncertainty arising from unknown physical properties) are calculated. 

The third component of our agent implements the shot selection from the set of candidates computed. We approach the problem as heuristic search in a game tree whose edges correspond to shots. When retrying a previously unsolved level the algorithm aims to find an alternative, previously untried sequence of shots to solve the level. Information from previous attempts is used to avoid ineffective shots or to redo effective shots in slightly different scenarios. Also a priority is set on plans that affect targets not destroyed in previous attempts. Shots are selected weighted randomly based on their expected score. Unlike classic game settings previously studied in AI, it is not possible to explore a significant portion of the search tree since exploration requires to engage in the game and time is limited.

In a fourth and last step, we decide once a level has been played, which level to try next. We select the level that is expected to yield the largest reward considering information about the type of level, the number of previous attempts, the points that might be earned, the set of shot candidates not yet tried. We apply machine learning to obtain models of the performance of our agent, given features above to tackle the selection problem as a k-armed bandit problem. Similar to shot selection, BamBirds suffer from the challenge that there is too little time available for performing a reasonable exploration phase. Therefore level and action selection hinge on effective heuristics that have been designed manually and by use of machine learning.

## BamBirds 2022 team members

The following students have worked on different parts of the agent, improving very narrow to very large parts. 

Some of the changes are not included in the submitted version since they are not yet stable enough.

### Case Based Reasoning
* Yannick Lang
* Oliver Schmidt

### Coordination
* Prof. Dr. Diedrich Wolter
* Felix Haase


## Changelog

### Features

- Case Based Shot Analysis (online): Save shot and its effects and adjust expected scores of other shots based on their similarity to it
- Case Based Reasoning (offline): Save the effects of a shot based on its impact angle to a small portion of a scene and reuse the information by applying it to other scenes by transformation
- Recursive support calculations for collapsing structures
- Categorize affected pigs of a plan by destroy, affect and free
- Calculate scores of shots based on affected pigs and the confidence
- Select shots using second order thompson sampling (from AgentX, credit to Daniel Lutalo from ANU)
- Target any object as new last resort strategy

### Bug Fixes

- Collisions with ball shaped objects
- Near misses because of target points on the outside of collision shapes
- Use white bird not in other strategies