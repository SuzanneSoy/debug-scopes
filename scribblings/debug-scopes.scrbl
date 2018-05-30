#lang scribble/manual
@require[scribble/example
         scribble-enhanced/doc
         @for-label[debug-scopes
                    racket/base
                    racket/contract]]

@title{Debuging scope-related issues}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@defmodule[debug-scopes]

@defproc[(+scopes [stx syntax?]) string?]{The identifiers are adorned with
 superscripts indicating the scopes present on them. Each scope is represented
 as an ascending integer which is unique within the current expansion. At the
 end of the expansion, a table showing the equivalence between the small
 integers and the scopes as represented by Racket is printed. Ranges of
 consecutive scopes are represented as @racket[identifier³˙˙⁹] (which would
 indicate that the scopes 3, 4, 5, 6, 7, 8 and 9 are present on the
 identifier). When only a few scopes are missing from the range, they are
 printed as @racket[identifier³˙˙⁹⁻⁵⁻⁷] (which would indicate that the scopes
 3, 4, 6, 8 and 9 are present on the identifier). When there are too many
 missing identifiers within the range, the scopes are instead displayed
 alternatively as superscripts and subscripts, e.g.
 @racket[identifier²₃⁵₇¹¹₁₃¹⁷₁₉] (which would indicate that only the scopes 2,
 3, 5, 7, 11, 17 and 19 are present on the identifier, and would also indicate
 that a developer is playing a trick on you). Finally the current macro scope
 (which can be removed using @racket[syntax-local-value]) and the current
 use-site scope, if any (which can be removed using
 @racket[syntax-local-identifier-as-binding]) is printed for the whole
 expression, using the notation @racket[(expression …)ˢˡⁱ⁼⁴⁺ᵘˢᵉ⁼¹²] (which
 would indicate that the macro scope is 4 and the use-site scope is 12).

 @examples[#:lang racket
 (require (for-syntax racket/base
                      debug-scopes))
 (define-syntax (foo stx)
   (displayln (+scopes stx))
   (displayln (+scopes (datum->syntax #f 'no-scopes)))
   (displayln (+scopes (syntax-local-introduce #'here)))
   (print-full-scopes)
   #'(void))

 (foo (list 123))]

 When using @racketmodname[debug-scopes/named-scopes], a named scope is often
 used instead of the macro scope flipped by @racket[syntax-local-introduce]. If
 @racket[+scopes] is called within that context, it also annotates the whole
 expression with the named scope which acts as a replacement for the macro
 scope, using the notation @racket[(expression …)ˢˡⁱ⁼⁴⁺ᵘˢᵉ⁼¹²⁽ⁿᵃᵐᵉᵈ⁼⁵⁾] (which
 would indicate that the original macro scope was 4, the use-site-scope is 12,
 and the named macro scope is 5).

@examples[#:lang racket
 (require (for-syntax (except-in racket/base syntax-local-introduce)
                      debug-scopes
                      debug-scopes/named-scopes))
 (define-syntax (foo stx)
   (displayln (+scopes stx))
   (displayln (+scopes (datum->syntax #f 'no-scopes)))
   (displayln (+scopes (syntax-local-introduce #'here)))
   (print-full-scopes)
   #'(void))

 (foo (list 123))]}

You can combine @racket[+scopes] with @racketmodname[racket/trace] to trace
scopes through expansion.
@examples[
  (require racket/trace (for-syntax racket/base racket/trace debug-scopes))
  (begin-for-syntax
    (define (maybe-syntax->scoped syn?)
      (if (syntax? syn?)
          (+scopes syn?)
          syn?))
    (current-trace-print-args
      (let ([ctpa (current-trace-print-args)])
        (lambda (s l kw l2 n)
          (ctpa s (map maybe-syntax->scoped l) kw l2 n))))
    (current-trace-print-results
      (let ([ctpr (current-trace-print-results)])
        (lambda (s l n)
         (ctpr s (map maybe-syntax->scoped l) n)))))
  (trace-define-syntax let
    (syntax-rules ()
      [(_ ([x v]) e) ((lambda (x) e) v)]))
  (let ([x 5]) (let ([y 120]) y))]

@defproc[(print-full-scopes [reset? any/c #t]) void?]{ Prints the long scope id
 and annotation for all scopes displayed as part of preceeding calls to
 @racket[+scopes], as would be shown by
 @racket[(hash-ref (syntax-debug-info stx) 'context)].

 This allows to get some extended information about the scopes in a summary
 table by calling @racket[print-full-scopes], while still getting short and
 readable display of syntax objects with @racket[+scopes].

 After running @racket[(print-full-scopes)], if @racket[reset?] is true, then
 the scope counter is reset (and @racket[+scopes] therefore starts numbering
 scopes starting from @racket[0] again).}

@section{Hack for named scopes}

@defmodule[debug-scopes/named-scopes/exptime]

Module scopes bear are annotated by Racket with the name of the module. As of
December 2016, other scopes like macro scopes@note{Both the ones implicitly
 created when a macro is called, and the ones explicitly created via
 @racket[make-syntax-introducer] are concerned by this} or use-site scopes lack
any form of annotation or naming.

@defproc[(make-named-scope [name (or/c string? symbol?)])
         (->* (syntax?) ([or/c 'add 'remove 'flip]) syntax?)]{ This function
 uses a hack to create named scopes on demand: it creates a dummy mododule with
 the desired name, expands it and extracts the module's scope. The exact
 implementation mechanism may vary in future versions, for example if later
 versions of Racket directly support the creation of named scopes,
 @racket[make-named-scope] would simply become an alias for the official
 mechanism. Later versions of this function may therefore produce named scopes
 other than module-like scopes.}

@defproc[(make-module-like-named-scope [name (or/c string? symbol?)])
         (->* (syntax?) ([or/c 'add 'remove 'flip]) syntax?)]{
 Produces a named module-like scope. The @racket[make-named-scope] function
 currently also produces a module-like scope, so the two are equivalent for now.
 In later versions, @racket[make-named-scope] may produce other sorts of named
 scopes if they can be created more efficiently, but
 @racket[make-module-like-named-scope] will always produce module-like scopes.}

@define[orig:define-syntax @racket[define-syntax]]
@define[orig:syntax-local-introduce @racket[syntax-local-introduce]]

@subsection{Automatic use of named scopes}

@defmodule[debug-scopes/named-scopes/override]

This module overrides @orig:define-syntax and @orig:syntax-local-introduce to
automatically use a named macro scope. The use-site scope is not affected for
now, as the original unnamed use-site scope from Racket benefits from special
cooperation from definition contexts, which would be hard to achieve with the
hack currently used to implement named scopes.

@defform*[((define-syntax (name stx-arg) . body)
           (define-syntax name value))]{

 Like @orig:define-syntax, but the first form changes the macro scope
 introduced by @racket[syntax-local-introduce] to use a named scope, bearing
 the @racket[name] of the macro.

 Note that this change only affects the scopes introduced by the overriden
 version of @racket[syntax-local-introduce], not the original
 @|orig:syntax-local-introduce|.

 This means that if the macro calls a function defined in another file which
 uses the non-overidden version of @orig:syntax-local-introduce, both the
 original unnamed scope and the named scope may accidentally appear in the
 result. Macros defined using the overridden @racket[syntax-local-introduce]
 should therefore take special care to always use the overridden version of
 @racket[syntax-local-introduce].

 The use-site scope is not affected for now, as the original unnamed use-site
 scope from Racket benefits from special cooperation from definition contexts,
 which would be hard to achieve with the hack currently used to implement named
 scopes.}

@defproc[(syntax-local-introduce [stx syntax?]) syntax?]{ Like
 @orig:syntax-local-introduce, but uses the named scope set up by
 @racket[define-syntax] if called within the dynamic extent of a call to a
 macro defined by the overridden @racket[define-syntax] (and otherwise behaves
 like the original @orig:syntax-local-introduce).}