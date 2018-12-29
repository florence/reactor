#lang scribble/manual
@(require (for-label (except-in racket last)
                     reactor)
          scribble/example)

@(define evil (make-base-eval '(require reactor)))
@(evil '(require racket/contract))



@bold{Warning:} This API is unstable, and may change without warning.

@title{Reactor: a synchronous reactive language}
@defmodule[reactor]

@(define synchronl "https://www-sop.inria.fr/mimosa/rp/generalPresentation/index.html")

Reactor is a @hyperlink[synchronl]{synchronous reactive}
language in the style of @hyperlink["http://rml.lri.fr/"]{
 ReactiveML}. A program is represented by a
@racket[reactor?].

The run of one program is broken up into @deftech{reactions} (also called @deftech{instants}), each of
which can be though of as being @deftech{instantaneous}---that is, absent side effects, no
concurrent thread of execution runs before or after any other thread. They all occur at the same time, and
so seem to take zero logical time. Thus, concurrent computations are deterministic.

Every expression in the program either takes zero time (e.g.
completes in the current reaction) or @deftech{pauses}, which "consumes" time, stopping the 
computation there until the next @tech{reaction}.

The code within a @racket[reactor?] can be a mix of both
reactive code (like @racket[par&]) and non-reactive code
(e.g. normal racket expressions). This non-reactive will
never consume time and should always terminate.

In general racket level side effects (mutation,
non-termination, etc) may break the guarantee of determininistic concurrencuy.

@section{Running Programs}

@defproc[(prime [proc procedure?]
                [args any/c ...]) reactor?]{
                                           
 Create a new @racket[reactor?], who's code is the body of @racket[proc]. Usually
 @racket[proc] is a @tech{reactive function}, although this is not necessary.
 The first reaction will invoke @racket[proc] with @racket[args].
 
}

@defproc[(react! [r (and/c reactor? reactor-safe?)]
                 [start-signals (or/c pure-signal? (list/c value-signal? (listof any/c)))] ...)
         any]{

 Run one @tech{reaction} in the @racket[r]. The reaction begins by
 @racket[emit&]ting the given @tech{signals} with the given values.

}

@section{Creating Reactive Functions}

@deftech{Reactive functions} may contain arbitrary racket code. In addition, it
may use the use the following forms. By convention forms and functions
ending in a @racket[&] may only be used within the dynamic extent of a 
@tech{reaction}.

@(define valid @list{Only valid within the dynamic extent of a @tech{reaction}.})

@defidform[pause&]{

 @tech{pause} the current @tech{reactive function} until the next reaction. Evaluates
 to @racket[(void)] in the next reaction.

 @valid

 @examples[#:eval evil
           (define (pause)
             pause&
             (displayln 1))
           (define r (prime pause))
           (react! r)
           (react! r)]

}

@defform[(par& e ...)]{

 Runs each @racket[e] concurrently. This expression completes evaluation
 when each new branch has finished.
 Evaluates to void.

 At the end of a reaction if all but one branch has
 completed that branch becomes in tail
 position with respect to the @racket[par&] form.

 @valid

 @examples[#:eval evil
           (define (par1)
             (displayln (par& 1 2)))
           (react! (prime par1))
           (define (par2)
             (displayln
              (par& 1
                    (begin pause& 2))))
           (define r (prime par2))
           (react! r)
           (react! r)]
                       
}

@defform[(loop& body ...)]{

 Loop @racket[body]s forever. The body of the loop must be
 non-@tech{instantaneous}: it must pause each @tech{instant} the loop
 (re)starts.

 @valid

 @examples[#:eval evil
           (define (loop)
             (let ([i 0])
               (loop& (displayln i)
                      (set! i (+ 1 i))
                      pause&)))
           (define r (prime loop))
           (react! r)
           (react! r)
           (react! r)
           (react! r)]
                        
}


@defidform[halt&]{

 @tech{pause} forever.

 @valid


 @examples[#:eval evil
           (define (halt)
             (displayln 1)
             halt&
             (displayln 2))
           (define r (prime halt))
           (react! r)
           (react! r)
           (react! r)
           (define (par-halt)
             (par& (begin (displayln 1) pause& (displayln 2))
                   (begin (displayln 3) halt& (displayln 4))))
           (define r2 (prime par-halt))
           (code:line (react! r2) (code:comment "note: these may display in either order, since printing is a side effect"))
           (react! r2)
           (react! r2)]
                  
}

@subsection{Signals}

@deftech{Signals} are the core communication mechanism both
within a Reactor, and between a reactor and its environment.
It is never safe to share a signal between two reactors.

Signals may be either @deftech{present} or @deftech{absent}
within a given instant---@tech{present} if the have been
@racket[emit&]ted during thie current reactor, and @tech{
 absent} if it is the end of a reaction and it has never been
@racket[emit&]ted. This means that forms like
@racket[present&] and @racket[await&] which look at the
@deftech{presence} of a signal must delay their choice to
the end of a reaction if the signal is to be @tech{absent}.

@defform*[((define-signal S)
           (define-signal S default ...+)
           (define-signal S default ...+ #:gather gather)
           (define-signal S default ...+ #:gather gather #:contract contract))]{

 Defines a new signal. The first variant defines a @deftech{pure
 signal}, with no value. They can only be @tech{present} or @tech{absent} The second and third variants define
 a @deftech{value carrying signal}, which may be @racket[emit&]ed along side values.
 The default values on the signal will be @racket[default].
 Multiple @tech[#:key "presence"]{emissions} of the signal will be combined with
 @racket[gather] which should be a associative procedure of
 twice as many arguments as the signal has values, and should
 return as many values. When the gather function is applied
 all of the values of one @tech[#:key "presence"]{emissions} with be supplied before
 the values of another, in order. If no gather function is
 provided an error is raised if the signal is emitted twice
 in the same instant. The value emitted on a signal can only
 be observed in the next instant. The value of a signal can be
 extracted via @racket[last], and forms like @racket[await&]
 which dispatch on the carried value.

 When @racket[contract] is supplied the signal is protected by that contract.
 See also @racket[signal/c].

 The defaut values, gather function, and contract may be supplied in any order.

 
                                                     
}

@defform*[((signal S e)
           (signal (S ...) e)
           (signal ([S default #:gather gather] ...) e))]{

 Analogous to @racket[let], but for @tech{signals}.
              
}

@defproc*[([(emit& [S pure-signal?]) void?]
           [(emit& [S value-signal?] [v any/c] ...) void?])]{

 Emits a @tech{signal} in the current instant, making it @tech{present}. If the signal
 carries values, they must be given.

 @valid
 
}

@defform[(present& S then else)]{

 Evaluates to @racket[then] if @racket[S] is @racket[emit&]ted in this
 @tech{instant}. Evaluates to @racket[else] in the next @tech{instant}
 otherwise.
 
 @valid
 
}

@deftogether[(@defform*[#:literals (or)
                        ((await& maybe-immediate maybe-count signal-expr)
                         (await& S [pattern body ...] ...+))
                        #:grammar ([signal-expr signal-or-list
                                    (or signal-or-list ...+)]
                                   [maybe-immediate (code:line) #:immediate]
                                   [maybe-count (code:line) (code:line #:immediate n)])
                        #:contracts
                        [(S signal?)
                         (signal-or-list (or/c signal? (listof signal?)))]]
               @defform[(await*& S [(pattern ...) body ...] ...+)
                        #:contracts
                        [(S signal?)]])]{

 Awaits the @tech[#:key "presence"]{emissions} of the @tech{signal} @racket[S]. In the
 @racket[#:immediate] variant, it may respond to the @tech{presence} of @racket[S] in
 the current @tech{instant}. Otherswise it always @tech{pauses} in the first
 @tech{instant}. The first variant evaluates to @racket[(void)].

 If @racket[#:count] is provided @racket[await&] awaits that
 many @tech[#:key "presence"]{emissions} of @racket[S] in as many @tech{instants}.

 If @racket[signal-expr] is an @racket[or] clause or a list of signals the await is triggered
 if any of the signals is @tech{present}. However if  @racket[#:count] is
 provided, multiple of these signals being @racket[emit&]ed counts as only one emission.
                                             
 If pattern clauses are provided, @racket[S] must be a @tech{value carrying
 signal}. In this case the value is matched against the given
 patterns in the @tech{reaction} after @racket[S] is emitted. It evaluates to the
 @racket[body] of the first match. If none match the form
 continues to await the signal. The @racket[await&] form matches only signals that
 carry a single value. The @racket[await*&] form can match many valued signals.

 @valid
 
}


@defproc[(last [S value-signal?]) any]{

 Gets the values of @racket[S] in the previous @tech{instant}.
                                        
}

@defproc[(last? [S signal?]) boolean?]{

 Was this signal emitted in the previous @tech{instant}?

}

@defproc[(default [S value-signal?]) any]{

 Gets the values that @racket[S] was initialized with.
                                        
}

@examples[#:eval evil
          (define-signal input)
          (define/contract (counter input chan)
            (reactive-> pure-signal? value-signal? none/c)
            (emit& chan 0)
            (loop&
             (await& #:immediate input)
             (emit& chan (add1 (last chan)))
             pause&))
          (define/contract (printloop chan)
            (reactive-> value-signal? none/c)
            (loop&
             (await& chan
                     [times
                      (printf "got total of ~a inputs\n" times)])))
          (define/contract (main input)
            (reactive-> pure-signal? none/c)
            (define-signal crosstalk 0 #:gather +)
            (par& (counter input crosstalk)
                  (printloop crosstalk)))
          (define r (prime main input))
          (react! r)
          (react! r)
          (react! r input)
          (react! r)
          (react! r)
          (react! r input)
          (react! r)]

@subsection{Control}

@defform[#:literals(or)
         (suspend& e ... #:unless signal-expr)
         #:grammar
         [(signal-expr signal-or-list
                       (or signal-or-list ...+))]
         #:contracts
          [(signal-or-list (or/c signal? (listof signal?)))]]{
                                      
 Runs @racket[e] unless @racket[S] any instant
 where @racket[signal-expr] is @racket[emit&]ted. Suspends the body, @tech{pause}ing the computation
 otherwise. Evaluates to its the result of body.

 If @racket[signal-expr] is either an @racket[or] clause or a list of signals then the
 suspend executes when any of the given signals are @tech{present}.

 @valid

 @examples[#:eval evil
           (define (hi unlock)
             (suspend&
              (loop& (displayln 'hello) pause&)
              #:unless unlock))
           (define-signal print)
           (define r (prime hi print))
           (react! r print)
           (react! r)
           (react! r)
           (react! r print)
           (reactor-suspended? r)]

}
 


@defform[(abort& e ... #:after S [pattern body ...] ...)]{

 Runs the body until the @tech{signal} @racket[S] is @racket[emit&]ted. If no pattern clauses are provided, the body is
 aborted in the next instant, and the form evaluates to @racket[(void)].

 If patterns are provided, they are matched against the
 value carried by @racket[S]. The form evaluates to the body
 of the first clauses that matches. If non match the
 execution of @racket[e] continues.

 If @racket[e] completes before @racket[S] is emitted and
 the body is aborted, the form evaluates to the result of @racket[e].

 @valid

 @examples[#:eval evil
           (define (annoying silence)
             (abort&
              (loop& (displayln "I know a song that gets on everybody's nerves") pause&)
              #:after silence))
           (define-signal off)
           (define r (prime annoying off))
           (react! r)
           (react! r)
           (react! r off)
           (react! r)
           (reactor-done? r)]

}


@defform[(with-handlers& body ... #:after-error [a b] ...)]{

 Like @racket[with-handlers], but that works with reactive
 machines. Specifically, whenever a raised exception is
 caught by a @racket[with-handlers&], it behaves like an
 abort, with body being aborted at the end of the current instant, and the
 corresponding handler is run in the next instant.

 If multiple errors are raised in the same instant they're
 handlers are run in parallel, and the result of each thread
 is collected into a list (as with @racket[par&]). The order
 of the list is not specified.

}

@section{Data}

@defproc[(signal? [S any]) boolean?]{

 Is @racket[S] a @tech{signal}?

}

@defproc[(pure-signal? [S any]) boolean?]{

 Is @racket[S] a @tech{pure signal} which carries no value?

}

@defproc[(value-signal? [S any]) boolean?]{

 Is @racket[S] a @tech{value carrying signal}?

}

@defform[(signal/c c ...)]{

 Creates a contract for value that contain
 @racket[c]. The no argument case is equivalent to @racket[pure-signal?].

 If form appears syntactically within @racket[#:contract]
 option of the @racket[define-signal] form the contract is
 checked when: values are @racket[emit&]ted, values are read
 from the signal (e.g. via @racket[last]), and when values
 are combined via the gather function. If the gather function
 violates its contract the positive party will be blamed.

 If the contract is attached via another form the contract
 barrier does not cover the gather function. Instead the
 contract behaves like box or channel contracts, and is only
 checked when the value is read from or written to.

}

@defproc[(signal=? [s1 signal?] [s2 signal?]) boolean?]{

 Are these the same signal? True if @racket[emit&]ting either signal
 would cause the other signal to be @tech{present}.

}

@defproc[(signal-name [s signal?])
         (and/c symbol? (not/c symbol-interned?))]{

 Gets a symbol who's string value is the name this signal
 was first created with, and which is @racket[eq?] to the
 @racket[signal-name] of another signal only if the two
 signals are @racket[signal=?].
                                                   
}

@defproc[(reactor? [r any]) boolean?]{

 Is @racket[r] a reactor?

}

@defproc[(reactor-suspended? [r reactor?]) boolean?]{

 Is @racket[r] completely suspended. That is, the reaction
 will immediatly @tech{pause} making no progress unless a
 signal is provided which will cause a @racket[suspend&] to
 execute its body, or a @racket[await&] to make progress, etc.

}

@defproc[(reactor-done? [r reactor?]) boolean?]{

 Is @racket[r] done. That is the @tech{reactive function} which which @racket[r]
 was created has returned.

}

@defform[(reactive-> dom ... range)
         #:contracts ([dom contract?]
                      [range contract?])]{
                        
 like @racket[->], but also restricts the function to be
 called only in the dynamic extent of a reaction.
}

@defproc[(reactor-safe? [r reactor?]) boolean?]{

 Can @racket[react!] be called directly on this reactor? It returns
 false if control escapes a reaction via an abort, exception
 or other control jump, or if a reaction is already running
 in a different thread.

 This check is not thread safe. 
                                               
}

@section{Continuation Marks}

Reactor provides the ability to get the current @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{continuation
 marks} from a paused @racket[reactor?]. However continuation
marks in reactor have fundamental difference from those in
racket: They are a tree rather than a list. This is because
@racket[par&] essentually forks the current continuation into
several branches. Therefor Reactor mimics the racket continuation marks API,
but extends it with trees.


@defproc[(reactor-continuation-marks [r (and/c reactor? reactor-safe?)])
         continuation-mark-set-tree?]{

 Gets the continuation marks for @racket[r]. This is the
 Reactor equivalent of @racket[continuation-marks].

}

@defproc[(continuation-mark-set-tree->tree [cmst continuation-mark-set-tree?] [key any/c])
         tree?]{
 Get a tree containing the marks for @racket[key]. The Reactor analog of @racket[continuation-mark-set->list].
}


@defproc[(continuation-mark-set-tree? [it any/c]) boolean?]{

 Is @racket[it] an continuation mark set tree, the representation of a
 the continuation marks from a reactor?

}

@deftogether[(@defproc[(tree? [it any/c]) boolean?]
               @defstruct*[branch ([values list?] [children tree?])
                           #:transparent
                           #:omit-constructor]
               @defstruct*[leaf ([values list?])
                           #:transparent
                           #:omit-constructor])]{

 The representation of a tree of continuation marks from a
 @racket[reactor?]. @racket[tree?] returns true for branches
 and leafs. This representation of marks is "top down": that
 is the first mark is the mark at the top of the continuation
 tree, and therefor the oldest mark. The is the opposet of racket's continuation mark
 lists, where the first value is the most recent mark.
                                                           
 A reactor without active @racket[par&]s will always be
 represented by a leaf. A reactor with an @racket[par&] with
 have a branch. The @racket[branch-values] will contain the
 continuation mark values from above the @racket[par&].
 @racket[branch-children] will contain a tree for each active
 branch of the par.

 Note that this mean there will always be more than one
 child of a branch: If a @racket[par&] has a single branch at
 the end of a reaction that branch will become in tail
 position w.r.t the enclosing context of the @racket[par&],
 removing the @racket[par&] itself.

}

@section{Caveats and unstable API's}


@bold{Warning:} The API's and behaviors presented here are especially unstable, and may
change without warning.


@subsection{Signals and Synchronization}

@tech{Signals} act as
@tech["synchronizable event" #:doc '(lib "scribblings/reference/reference.scrbl")],
which becomes ready for synchronization at the end of a
@tech{reaction} in which the signal was @racket[emit&]ted. The
@tech["synchronization result" #:doc '(lib "scribblings/reference/reference.scrbl")]
is the signal itself.

Signals are currently not thread safe: if a signal is used
(via @racket[last?] or @racket[last]) in a thread different
from a reaction where it is being used, extra
synchronization must be used to ensure the signal is not
look at during a reaction.

@subsection{Caveat concerning exception handling and control jumps}

Catching an exception using @racket[with-handlers]
or @racket[call-with-exception-handler], capturing and 
and applying a continuation inside of a reaction, or
aborting to a prompt inside of a reaction is unsafe if
any of these cross a continuation containing any reactive form and do not
jump completely outside of the reaction. For
example, if an exception passed through a @racket[par&],
@racket[suspend&], or @racket[abort&] the reactors
control structure may become corrupted, and the reaction
behavior and the state of its signals is undefined.
Control may safely leave the reactor in this way, but the
reactor is marked as unsafe.

However, Catching exceptions with @racket[with-handlers&] is safe.




