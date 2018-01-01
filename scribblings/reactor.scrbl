#lang scribble/manual
@(require (for-label (except-in racket process last)
                     reactor
                     reactor/engine)
          scribble/example)

@(define evil (make-base-eval '(require reactor)))

@title{Reactor: A synchronous reactive language}
@defmodule[reactor]

@(define synchronl "https://www-sop.inria.fr/mimosa/rp/generalPresentation/index.html")

Reactor is a @hyperlink[synchronl]{synchronous reactive}
language in the style of @hyperlink["http://rml.lri.fr/"]{
 ReactiveML}. A program is represented by a
@racket[reactor?], which consists of @racket[process?]es.
The run of one program is broken up into reactions, each of
which can be though of as being instantaneous. Every
expression in the program either takes zero time (e.g.
completes in the current reaction) or pauses until the next
reaction.

@section{Running Programs}

@defproc[(prime [proc process?]) reactor?]{
                                           
 Create a new reactor, primed the one process.
 
}

@defproc[(react! [r (and/c reactor? reactor-safe?)]
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

 These processes are not related to Rackets
 @seclink["subprocess" #:doc '(lib "scribblings/reference/reference.scrbl")]{processes}.
 
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

@defidform[pause&]{

 Block the current process until the next reaction. Evaluates
 to @racket[(void)] in the next reaction.

 @valid

 @examples[#:eval evil
           (define-process pause
             pause&
             (displayln 1))
           (define r (prime pause))
           (react! r)
           (react! r)]

}

@defform[(par& e ...)]{

 Runs each @racket[e] as an independent process. This blocks
 the current process until each new process has finished.
 Evaluates to a list containing the result of each
 expression.

 @valid

 @examples[#:eval evil
           (define-process par1
             (displayln (par& 1 2)))
           (react! (prime par1))
           (define-process par2
             (displayln
              (par& 1
                    (begin pause& 2))))
           (define r (prime par2))
           (react! r)
           (react! r)]
                       
}

@defform[(loop& body ...)]{

 Loop @racket[body]s forever. The body of the loop must be
 non-instantaneous: it must pause each instant the loop
 (re)starts.

 @valid

 @examples[#:eval evil
           (define-process loop
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

 Suspends the current process indefinitely.

 @valid


 @examples[#:eval evil
           (define-process halt
             (displayln 1)
             halt&
             (displayln 2))
           (define r (prime halt))
           (react! r)
           (react! r)
           (react! r)
           (define-process par-halt
             (par& (begin (displayln 1) pause& (displayln 2))
                   (begin (displayln 3) halt& (displayln 4))))
           (define r2 (prime par-halt))
           (code:line (react! r2) (code:comment "note: these may display in either order"))
           (react! r2)
           (react! r2)]
                  
}


@defform[(run& proc)]{
                      
 Start the given process within the current one, blocking
 the process until it completes. Evaluates to the result
 of the process.

 @valid

 @examples[#:eval evil
           (define-process (my-loop i)
             (displayln i)
             pause&
             (run& (my-loop (add1 i))))
           (define r (prime (my-loop 0)))
           (react! r)
           (react! r)
           (react! r)]
                   

}

@subsection{Signals}

@deftech{Signals} are the core communication mechanism both
within a Reactor, and between a reactor and its environment.
It is never safe to share a signal between two reactors.

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
 carries a value, one must be given.

 @valid
 
}

@defform[(present& S then else)]{

 Evaluates to @racket[then] if @racket[S] is emitted in this
 instant. Evaluates to @racket[else] in the next instant
 otherwise.
 
 @valid
 
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


@examples[#:eval evil
          (define-signal input)
          (code:comment "pure-signal -> process")
          (define-process (main input)
            (define-signal crosstalk 0 #:gather +)
            (par& (run& (counter input crosstalk))
                  (run& (printloop crosstalk))))
          (code:comment "pure-signal? value-signal? -> process")
          (define-process (counter input chan)
            (emit& chan 0)
            (loop&
             (await& #:immediate input)
             (emit& chan (add1 (last chan)))
             pause&))
          (code:comment "value-signal? integer -> process")
          (define-process (printloop chan)
            (loop&
             (await& chan
                     [times
                      (printf "got total of ~a inputs" times)])))
          (define r (prime (main input)))
          (react! r)
          (react! r)
          (react! r input)
          (react! r)
          (react! r)
          (react! r input)
          (react! r)]

@subsection{Control}

@defform[(suspend& e ... #:unless S)]{
                                      
 Runs @racket[e] unless @racket[S] any instant
 where @racket[S] is emitted. Suspends the body and blocks
 otherwise. Evaluates to its the result of body.

 @valid

 @examples[#:eval evil
           (define-process (hi unlock)
             (suspend&
              (loop& (displayln 'hello) pause&)
              #:unless unlock))
           (define-signal print)
           (define r (prime (hi print)))
           (react! r print)
           (react! r)
           (react! r)
           (react! r print)
           (reactor-suspended? r)]

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

 @examples[#:eval evil
           (define-process (annoying silence)
             (abort&
              (loop& (displayln "I know a song that gets on everybody's nerves") pause&)
              #:after silence))
           (define-signal off)
           (define r (prime (annoying off)))
           (react! r)
           (react! r)
           (react! r off)
           (react! r)
           (reactor-done? r)]

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

 Is @racket[r] completely suspended. That is, there are no
 processes queued to immediately run on the next reaction, but
 there are some processes suspended on a signal

}

@defproc[(reactor-done? [r reactor?]) boolean?]{

 Is @racket[r] done. That is, are there processes queued to
 immediately run on the next reaction and no suspensions
 waiting blocked on a signal?

}

@defproc[(process? [p any]) boolean?]{

 Is @racket[p] a process?

}

@defproc[(reactor-safe? [r reactor?]) boolean?]{

 Can `react!` be called directly on this reactor? It returns false
 if any uncaught exception is raised in a reaction.

 See @secref["r&t"] for unstable behavior of this function.

                                                
}

@section{Caveats and unstable API's}


@bold{Warning:} The API's and behaviors presented here is unstable, and may
change without warning.

@subsection[#:tag "r&t"]{Reactions and Threads}

Reactors are not @racket[reactor-safe?] if either the
reactor is under the control of an ignition thread, or if
this reactor was created in a different thread.

@subsection{Signals and Synchronization}

Signals act as
@tech["synchronizable event" #:doc '(lib "scribblings/reference/reference.scrbl")],
which becomes ready for synchronization at the end of a
reaction in which the signal was emitted. The
@tech["synchronization result" #:doc '(lib "scribblings/reference/reference.scrbl")]
is the signal itself.

Signals are currently not thread safe: if a signal is used
(via @racket[last?] or @racket[last]) in a thread different
from a reaction where it is being used, extra
synchronization must be used to ensure the signal is not
look at during a reaction.

@subsection{Caveat concerning exception handling}

Catching an exception inside of a reaction is currently
unsafe. For example, if an exception passed through a
@racket[par&], @racket[suspend&], or @racket[abort&] for the
reactors control structure may become corrupted, and the
reaction may block forever, or raise internal errors.

In addition if an exception is raised within a reaction, the
state of any signals it could have effected are indeterminate.

@subsection{Engine}

@defmodule[reactor/engine]

@defproc[(ignition! [r (and/c reactor? reactor-safe?)])
         any]{

 Run reactions in the reactor automatically (in a different
 @tech["thread" #:doc '(lib "scribblings/reference/reference.scrbl")]).

 A reaction runs if there is a queued reaction or if any
 process @racket[pause&]ed in the last reaction---that is, if
 @racket[reactor-done?] and @racket[reactor-suspended?] are @racket[#f].

 This function causes @racket[(reactor-ignited? r)] be true,
 and @racket[(reactor-safe? r)] to be false.
                                                         
}

@defproc[(shutdown! [r (and/c reactor? reactor-ignited?)]) any]{

 Stop running reactions automatically. Causes
 @racket[(reactor-ignited? r)] to be false.
 @racket[(reactor-safe? r)] may still be false if the reactor
 crashed.

}

@defproc[(reactor-ignited? [r reactor?]) boolean?]{

 Has this reactor been ignited by @racket[ignition!].
 Implies @racket[(not (reactor-safe? r))].
                                                   
}

@defproc[(queue-emission!
          [r (and/c reactor? reactor-ignited?)]
          [emissions (or/c pure-signal? (list/c value-signal? any/c))] ...)
         any]{

 Queue a reaction in this reactor, with the given signals
 emitted. If this function is called multiple times before
 the queued reaction runs, all the given @racket[emissions]
 occur in the next instant.
 
}

@defproc[(bind-signal [r (and/c reactor? reactor-ignited?)]
                      [s signal?]
                      [e evt?])
         evt?]{

 Create an event that is ready when @racket[e] is ready, but
 who's synchronization result is itself. When
 synchronized on @racket[s] is queued for emission in
 @racket[r]. If @racket[s] is a @racket[value-signal?], it is
 emitted with the
 @tech["synchronization result" #:doc '(lib "scribblings/reference/reference.scrbl")]
 of @racket[e].

 If @racket[r] is @racket[shutdown!], this event will never
 become ready, and never queue an emission, even if the reactor is reignited.

 @bold{Warning:} Even if @racket[r] is @racket[shutdown!],
 synchronizing on the result of @racket[bind-signal] could
 still synchronize on @racket[e]. For example, if @racket[e]
 is a
 @tech["semaphore"  #:doc '(lib "scribblings/reference/reference.scrbl")],
 synchronizing on the result of @racket[bind-signal] might
 decrement the semaphores counter, even though the event is
 never ready.

}



