Required Knowledge: basic PLT, first-order logic, DFS, SLD resolution or similar methods, syntactic unification

Time Complexity: unbounded (language is Turing-complete)

The WatLog language is based on Prolog, and is essentially a very small subset of it. The major differences are that Prolog implementations tend to come with effectful features, cuts limiting the search space and negation as failure, only resolve the queries until a single satisfying variable assignment is found -- unless asked for more -- do not require all variable assignments to be closed, and do not perform the occurs check. Modern implementations of logic programming languages or libraries also tend to be much more sophisticated than the simple approach expected in this problem.

The lexing and parsing of the input should be straightforward, as the grammar is specifically designed to make that part easy, so I will not discuss it in detail.

The core problem of trying to prove query (or "goal") clauses is not particularly hard either: the constituent terms should be matched against the consequents of all known rules in the order specified in the problem statement -- left-to-right with respect to query terms, top-down with respect to rules. If a query term can be unified with the consequent of a given rule, it can be replaced by the premises of said rule in the query, after which the process repeats. Failure to prove a term leads to backtracking. The algorithm essentially explores the derivation space by DFS. Note that there is no need to implement any optimizations for quickly identifying candidate rules -- a naive implementation considering all inference rules in order should suffice to pass the test cases.

For theoretical background see:

http://en.wikipedia.org/wiki/SLD_resolution

Equality rules as described in the problem statement define first-order syntactic unification of terms. In WatLog's context the implementation is straightforward. WatLog differs from industry-strength logic programming languages in that it requires an occurs check.

See also:

http://en.wikipedia.org/wiki/Unification_(computer_science)

The more challenging part of the problem is dealing with variables and equality/non-equality assertions.

Equality assertion between a variable and another term (or unification of a variable with another term in context of unifying query terms with consequents) effectively performs an assignment. The easiest way to do that is term rewriting, simply replacing the assigned variable everywhere in the current state with the other side of the equation. Once the substitution has been performed, the assertion can be safely discarded. Note that one pitfall in treatment of variables is scoping. Variables of the same name in different rules, or in a single rule applied multiple times, should not be treated as the same variable. This problem can be solved using approaches similar to de Bruijn indices, or more simply, by automatic renaming using unique (and syntactically invalid to prevent clashes with query variables) affixes. Some care is needed to keep track of the variables that we must report to the user in case we find a satisfying derivation of the query.

Lastly, the non-equality assertions represent perhaps the biggest challenge here, as they cannot be simply processed and discarded. Non-equality assertions should be kept in the constraint storage until proven to hold -- by substituting all the variables involved so that both sides become simple terms. Failure to terminate search and backtrack as soon as an assertion can be proven not to hold may easily cause infinite recursion.

The reference implementation is listed below. (It's worth noting that it's not exactly a "model" solution in true sense of the word, as it has some quirks, such as the use of two different functions for term unification for no good reason.)
