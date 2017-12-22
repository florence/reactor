#lang scribble/manual
@(require (for-label (except-in racket process last)
                     reactor))

@title{Reactor: A synchronous reactive language}
@defmodule[reactor]

@bold{Warning:} The API presented here is unstable, and may
change without warning.

@section{Running Programs}
@defproc[(prime [proc process?]) reactor?]{
                                           
 Create a new reactor, primed the one thread.
 
}

@defproc[(react! [r reactor?]
                 [start-signals (or/c pure-signal? (list/c value-signal? any/c))] ...)
         any]{

 Run one reaction in the reactor. The reactions begins by
 emitting the given signals with the given value.

 @bold{Warning:} @racket[react!] is not thread safe. Two
 reactions in @racket[r] should not take place concurrently,
 and @tech{signals} used by @racket[r] should not be observed
 outside of @racket[r] during a reaction.

}

@section{Creating Processes}

@defform*[((define-process id body ...)
           (define-process (id args ...) body ...))]{
                                                      
 Define new process.
                                                      
 The second variant creates a new function, @racket[func]
 which takes @racket[args] and returns the process defined by
 @racket[body].
 
}

@defform[(process body ...)]{
 Create a new process.
}

@section{Defining Processes}

A process may contain arbitrary racket code. In addition, it
may use the use the following forms. By convention forms
ending in a @racket[&] may only be used inside of a
@racket[process] or @racket[define-process].

@(define valid @list{Only valid inside
 of a @racket[process] or @racket[define-process].})

@defform[(run& proc)]{
                      
 Start the given process within the current one, blocking
 the thread until it completes. Evaluates to the result
 of the process. @valid
 
}

@defform[(par& e ...)]{

 Runs each @racket[e] independently in the current process.
 This blocks the current process until each new thread has
 finished. Evaluates to a list containing the result of each
 expression. @valid
                       
}

@defform[(loop& body ...)]{

 Loop @racket[body]s forever. The body of the loop must be
 non-instantaneous: it must pause each instant the loop
 (re)starts. @valid
                        
}

@defidform[paused&]{

 Block the current thread until the next reaction. evaluates
 to @racket[(void)] in the next reaction. @valid

}

@defidform[halt&]{

 Blocks this thread forever. @valid
                  
}

@subsection{Signals}

@deftech{Signals} are the core communication mechanism within a Reactor.

@defform*[((define-signal S)
           (define-signal S default #:gather gather))]{

 Defines a new signal. The first variant defines a pure
 signal, with no value. The second variant defines a value-carrying signal.
 The default value on the signal will be @racket[default]. Multiple emissions
 of the signal will be combined with @racket[gather] which should be a
 associative procedure of two arguments. The value emitted on a signal can only
 be observe in the next instant.
                                                     
}

@defform*[((signal S e)
           (signal (S ...) e)
           (signal ([S default #:gather gather] ...) e))]{

 Analogous to @racket[let], but for signals.
              
}

@defproc*[([(emit& [S pure-signal?]) void?]
           [(emit& [S value-signal?] [v any]) void?])]{

 Emits a signal in the current instant. If the signal
 carries a value, one must be given. @valid
 
}

@defform[(present& S then else)]{

 Evaluates to @racket[then] if @racket[S] is emitted in this
 instant. Evaluates to @racket[else] in the next instant
 otherwise.
                                 
}

@defform*[((await& maybe-immediate S)
           (await& S [pattern body ...] ...+))
          #:grammar ([maybe-immediate (code:line) #:immediate])]{

 Awaits the emission of the signal @racket[S]. In the
 @racket[#:immediate] variant, the unblocks the same instant
 @racket[S] is emitted, and evaluates to @racket[(void)].
 Otherwise it terminates the following instant.
                                             
 If pattern clauses are provided, @racket[S] must be a value carrying
 signal. In this case the value is matched against the given
 patterns in the reaction after @racket[S] is emitted. It evaluates to the
 @racket[body] of the first match. If none match the form
 continues to await the signal.

 @valid
 
}

@defproc[(last [S value-signal?]) any]{

 Gets the value of @racket[S] in the previous instant.
                                        
}

@defproc[(last? [S signal?]) boolean?]{

 Was this signal emitted in the previous instant?

}

@subsection{Control}

@defform[(suspend& e ... #:unless S)]{
                                      
 Runs @racket[e] unless @racket[S] any instant
 where @racket[S] is emitted. Suspends the body and blocks
 otherwise. Evaluates to its the result of body. @valid
 
}

@defform[(abort& e ... #:after S [pattern body ...] ...)]{

 Runs the body until @racket[S] is emitted. If no pattern clauses are provided, the body is
 aborted in the next instant, and the form evaluates to @racket[(void)].

 If patterns are provided, they are matched against the
 value carried by @racket[S]. The form evaluates to the body
 of the first clauses that matches. If non match the
 execution of @racket[e] continues.

 If @racket[e] completes before @racket[S] is emitted and
 the body is aborted, the form evaluates to the result of @racket[e].

 @valid

}

@section{Data}

@defproc[(signal? [S any]) boolean?]{

 Is @racket[S] a signal?

}

@defproc[(pure-signal? [S any]) boolean?]{

 Is @racket[S] a signal which carries no value?

}

@defproc[(value-signal? [S any]) boolean?]{

 Is @racket[S] a signal which carries a value?

}

@defproc[(reactor? [r any]) boolean?]{

 Is @racket[r] a reactor?

}

@defproc[(reactor-suspended? [r reactor?]) boolean?]{

 Is @racket[r] completely suspended. That is, are there
 threads queued to immediately run on the next reaction?

}

@defproc[(reactor-done? [r reactor?]) boolean?]{

 Is @racket[r] done. That is, are there threads queued to
 immediately run on the next reaction and no suspensions
 waiting blocked on a signal?

}

@defproc[(process? [p any]) boolean?]{

 Is @racket[p] a process?

}