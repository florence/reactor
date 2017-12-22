#lang scribble/manual
@(require (for-label (except-in racket process last)
                     reactor))

@title{Reactor: A synchronous reactive language}
@defmodule[reactor]

@bold{Warning:} The API presented here is unstable, and may
change without warning.

@section{Running Programs}
@defproc[(start [proc process?]) reactor?]{
 Create a new reactor with a single thread.
}

@defproc[(react [r reactor?]
                [start-signals (or/c pure-signal? (list/c value-signal? any/c))] ...)
         any]{

 Run one reaction in the reactor. The reactions begins by
 emitting the given signals with the given value.

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
                      
 Start the given process within the
 current one, blocking until it completes. @valid
 
}

@defform[(par& e ...)]{

 Runs each @racket[e] independently in the current process. This blocks
 the current process until each new thread has finished. @valid
                       
}

@defform[(loop& body ...)]{

 Loop @racket[body]s forever. The body of the loop must be
 non-instantaneous: it must pause each instant the loop
 (re)starts. @valid
                        
}

@defidform[paused&]{
  Block the current thread until the next reaction. @valid
}

@defidform[halt&]{

 Block this thread forever. @valid
                  
}

@subsection{Signals}

@defform*[((define-signal S)
           (define-signal S default #:gather gather))]{

 Defines a new signal. The first variant defines a pure
 signal, with no value. The second variant defines a value-carrying signal.
 The default value on the signal will be @racket[default]. Multiple emissions
 of the signal will be combined with @racket[gather] which should be a
 associative procedure of two arguments.
                                                     
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

 Executes @racket[then] if @racket[S] is emitted in this
 instant. Executes @racket[else] in the next instant
 otherwise.
                                 
}

@defform*[((await& #:immediate S)
           (await& S [pattern body ...] ...))]{

 Awaits the emission of the signal @racket[S]. In the
 @racket[#:immediate] variant, the unblocks the same instant
 @racket[S] is emitted.
                                             
 In the second variant @racket[S] must be a value carrying
 signal. In this case the value is matched against the given patterns
 in the next instant. The body of the first match is run. If none match
 the form continues await the signal.

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
                                      
 Runs the body of the suspend unless @racket[S] any instant
 where @racket[S] is emitted. Suspends the body and blocks
 otherwise. @valid
 
}

@defform[(abort& e ... #:after S)]{

 Runs the body until @racket[S] is emitted. The body is then
 aborted in the next instant. @valid

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

@defproc[(reactor-done? [r reactor?]) boolean?]{

 Is @racket[r] done. That is, are there no more remaining threads?

}

@defproc[(process? [p any]) boolean?]{

 Is @racket[p] a runnable process?

}