Please fill out each TODO item in the header.

Please don't reformat the HEADER area of this file, and change nothing
except the TODOs, particularly nothing before the colon ":" on each
line!

We reserve the right to do (minor) point deductions for misformatted
READMEs.

===================== HEADER ========================

Student #1, Name: Aine Kearns
Student #1, students.cs.ubc.ca Login: f0a0b
Student #1, Student Number: 45402147

Student #2, Name: NONE
Student #2, students.cs.ubc.ca Login: NONE
Student #2, Student Number: NONE

Team name (for fun!): Free Solo

Acknowledgment that you understand and have followed the course's
collaboration policy (READ IT at
https://www.students.cs.ubc.ca/~cs-311/current/syllabus.html#collaboration-policy-for-assignments):

Signed: Aine Kearns

===================== LOGISTICS =====================

Please fill in each of the following:

Acknowledgment of assistance (per the collab policy!): Lilli Freischem, Kaeli Flanagan, Laura Schmid

For teams, rough breakdown of work: NONE

====================== THEORY =======================

1. Explain why changing `fun` to `byname-fun` can change cause
a program to produce a different result. 

Changing it to by-name fun would result in a different kind of function evaluation. It would mean that the argument passed into the function wouldn't be evaluated until that code is hit in the function evaluation. 

2. `begin` is desugared into `with`, which is desugared using `fun`.
Given that our language has mutation, could we desugar `begin` into `lazy-with`?
Why or why not?    

Yes, we could desugar `begin` into `lazy-with` because we could mutate a single value with the results of each line of the begin. Lazy-with will put them together, execute them all, and return the result of the final with while still making the changes in each line.

3. We used an immutable data structure (stores) to implement a language
with mutable state. Whenever a (setvar!) was performed, we created a new store
without altering the old one. This is, in general, slower than using Racket's mutable state.

Name a language feature, or a feature of a tool (debugger, IDE, etc.) 
that could be added to A3L or a similar language, that can be done
using immutable stores, that can't be done with Racket's mutable state. 




======================= BONUS =======================

If you attempted any bonuses, please note it here and describe how you
approached them.

TODO