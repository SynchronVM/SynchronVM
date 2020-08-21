## MIDDLEWARE

This is the middleware of the Sense-VM project. Here we expose a Core Language which is Lambda Calculus extended with recursive let bindings and other syntactic sugar. We currently experiment with the call-by-value evaluation strategy using the Categorical Abstract Machine. There exists other abstract machines like Krivine Machine, ZINC etc which can be experimented with this core.

Another option could be evaluating the core language using SK combinators or STG machine to experiment with various call-by-name strategies. Each abstract machine implementation should contain atleast the following:

- Interpreter (for Core to abstact machine assembly)
- Assembler (for abstract machine assembly to bytecode)
- Debugger (for debugging the abstract machine assembly)
