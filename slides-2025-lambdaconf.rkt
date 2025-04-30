#lang at-exp racket @; -*- Scheme -*-
#|
Orthogonal Persistence, the Model

Slides for presentation at LambdaConf 2025 in Estes Park, Colorado, 2025-05-12

To compile it, use:
  mkdir -p run
  racket slides-2025-lambdaconf.rkt > run/slides-2025-lambdaconf.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket

This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
         "util/util.rkt"
         "util/coop.rkt"
         (rename-in "util/coop.rkt" (|@| $))
         "util/protodoc.rkt"
         "util/reveal.rkt")

(def doc
  (docfix
    ($title "Orthogonal Persistence, the Model")
    ($kv 'slide
      (list
       @div[class: 'logo]{
       @img[src: "resources/pic/mukn-name.svg"
            alt: "Mutual Knowledge Systems"
            width: "50%"
            valign: 'middle
            style: "
    vertical-align: middle;
    background-color: white;
    padding-left: .5em;
    padding-right: .5em;
    padding-top: .5em;
    padding-bottom: .5em;
"]}
       @; TODO: hide the menu behind a button at the top when on mobile (?)
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 140%"
            @b{Orthogonal Persistence: @br the Model}]
       @(br clear: 'all)
@;       @p{@small{@(~)}}
@;       @L{@code{@(~ 18) build = λ b m ↦ Y (m b)} @br
@;          @code{@(~ 18) inherit = λ p c u s ↦ c (p u s) s}}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{http://github.com/mighty-gerbils/gerbil-persist}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{LambdaConf 2025-05-12}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Prelude: Orthogonal Persistence?"
     $plan-slide
     ($slide "A Simple Story"
        @;{
             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater,
             is sitting on a chair at a café terrace in San Theodoros,
             with a view on the beach of San Theodoros.
             He is on the bottom left of the panel;
             we see his back, he is turned slightly towards the right of the panel.
             His is typing on a laptop on the round café table.
             On the laptop, a white-on-black window on which is display "> foo = 42"
             and at the next line "> " aligned under the above ">", followed by a blinking underscore cursor.
             On the top left side of the panel,
             a robber in a hoodie is approaching subreptitiously;
             you can't see the robber's face, hidden under his hood.
             In the background, there is a scene of poverty and corruption in the streets of San Theodoros.

             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater,
             was sitting at a café terrace in San Theodoros,
             with a view on the beach of San Theodoros.
             He is on the bottom left of the panel;
             we see his back, he is turned slightly towards the right of the panel.
             he is half-standing instead of sitting, the round café table is falling in front of him.
             A robber stole the laptop and is running away with it,
             away from Captain Haddock; he is on the right of the panel, and
             heading further towards the top right of the panel.
             Captain Haddock, is cursing "Buccaneer!"
             and other censored curses, his fist raised;
             the speech bubble points at Captain Haddock.
             In the background, there is a continuation of the previous scene of poverty and corruption
             in the streets of San Theodoros.

             Generate a comic panel in the genre of Hergé.
             We see Captain Haddock with a unicorn on his sweater, flabbergasted,
             three-quarter facing us, turned slightly towards the right of the panel,
             calling a customer support line.
             Haddock is holding a newspaper, the San Theodoros Times, that sports a headline:
             "Nuevo-Rican missile hits datacenter".
             The voice on the phone, with a speech bubble pointing at the listening part of the phone,
             tells him:
             "We didn't lose just your data. All data is gone, and we lost many engineers."

             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater,
             is grumbling, sitting laptopless on a plane from San Theodoros back to Marlinspike.
             He is three quarter facing us and towards the right of the panel.
             On the porthole behind him, you can see the wing of the plane.
             The picture of Haddock is itself but an insert in a background map
             that shows the plane itinerary, going FROM South America (not Africa), back TO Europe.

             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater, home at Marlinspike Hall,
             is on the grand porch walking from the inside.
             He is opening his arms and effusively greets a tech support agent
             bringing a white carton box with a picture of a laptop on it.
             As he does so, he pushes aside the Marlinskpike butler Nestor
             who was giving the agent a hard time.
             Haddock says: "Ah, my savior!"
             The agent, at the bottom of the steps to the porch,
             hands the box to Haddock.
             The agent declares: "It's preinstalled, just enter your credentials"

             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater,
             was sitting on a chair at a terrace in his garden at Marlinspike Hall.
             He is on the bottom left of the panel;
             we see his back, he is turned slightly towards the right of the panel.
             On the table he has a laptop,
             with white-on-black window on which is display "> foo"
             and at the next line "42" aligned under the above ">",
             and yet on the next line, a prompt "> " aligned as well, followed by a blinking underscore cursor.
             The captain says contentedly: "Yes! It's still there."
             On the top left side of the panel,
             Nestor the Marlinskpike butler is bringing the captain a bottle of whisky and a glass on a platter.
        ;}
        @L{At a REPL on your laptop, you define @code{foo}}
        @L{You involuntarily donate your laptop to a “youth”}
        @L{Orcs raze your data center}
        @L{Back home, install new laptop, enter credentials}
        @L{Your session is as before, @code{foo} still defined}
        @L{... yet not a single LoC involved backing up data})
     ($slide "Persistence vs What?"
        @L{Persistence is @em{when your data survives}}
        @L{As with Security: what attack model?}
        @L{Unexpected: power loss, accident, theft, fire, war}
        @L{Expected: hardware and software end-of-life})
     ($slide @list{History}
        @L{OODB 1980s-now: Lisp, Smalltalk, C++, Java...} ;; Continuations don't persist!
        @L{1980s-1990s: EUMEL, EROS… Little research 2000s-2010s.}
        @L{Today “Durable Execution”. Renewed research.}
        @L{Me: My good old HP28})
     ($slide @list{Thesis}
        @L{Orthogonal Persistence (OP) is the Right Thing™}
        @L{Persisting Continuations simplifies everything else}
        @L{Transactions are the wrong abstraction}
        @L{OP elevates the problems it doesn’t solve}))
    ($section "Today: Fractal Transience"
     $plan-slide
     ($slide @list{Disappointing Users}
        @L{My mom's book database}
        @L{C-x C-s in my fingers}
        @L{Who never lost data to no/bad/lost backups?}
        @L{... Don't modern Apps autopersist data?})
     ($slide @list{Vendor Lock-In Persistence}
        @L{Only easy case: won't sync across devices, accounts...}
        @L{Forced Code Upgrade or Lack Thereof}
        @L{Product Cancellation, Change of Ownership, Bankruptcy}
        @L{Data Hostage: pay bills & pray Vendor healthy})
     ($slide @list{Developers: Hard Mode}
        @L{Computers, processes, etc., start empty}
        @L{Explicitly save and restore all data} ;; old IBM study: 30% is (un)marshalling. Not counting error recovery.
        @L{Storage Zoo: FS, DB, Web, git...}
        @L{Fake it the hard way, and Admin the mess})
     ($slide @list{Hard Problems}
        @L{Managing Many Out-of-Sync Copies}
        @L{Fragile Backups}
        @L{Schema Upgrades}
        @L{Merging Data})
     ($slide "What If...?"
        @L{Persistence was the default}
        @L{No more vendor lock-in}
        @L{Developers don't have to care... except specialists}
        @L{Hard problems are made easier somehow?}))
    ($section "Orthogonal Persistence for Users"
     $plan-slide
     ($slide @list{It's Just There}
        @L{Data “just” persists}
        @L{Code runs in ambient “domain”}
        @L{Interactions, e.g. Editing Documents, not “Apps”}
        @L{Backup Storage Provider Subscription}) ;; Admin role, can be delegated to accountable party
     ($slide @list{Example Domains}
        @code{
          (me (work (client1 client2 client3 hr)) @bri
              (home (cooking kids taxes)) @bri
              (secret (cia kgb mosad)) @bri
              (love (wife lover1 lover2)))
        })
     ($slide @list{Domains}
        @L{User creates sub-domains}
        @L{Origin Domain Validates Copy Out / Share Out}
        @L{Destination Domain Validates Paste In / Share In}
        @L{Attach Storage Providers})
     ($slide @list{Document}
        @L{Edit it}
        @L{Always Saved}
        @L{Not always publishable}
        @L{Share some versions with some people, one way or both}))
    ($section "Orthogonal Persistence for Developers"
     $plan-slide
     ($slide @list{Just Do It}
        @L{Variables “just” persist}
        @L{Eliminate 30%-70% of all code}
        @L{No more save and restore}
        @L{Literally torture and shoot dead anyone who tries})
     ($slide @list{Capabilities: Photo App}
        @L{Photo App: save... over net... download extensions} ;; also camera, etc.
        @L{Regular App: will compromise your entire computer}
        @L{Persistent App: can only edit given picture}
        @L{Wider Reflective Capability Architecture})
     ($slide @list{Security Improvement}
        @L{Fewer things to think about} ;; for AIs as well as humans
        @L{Fewer things to get wrong}
        @L{Smaller attack surface}
        @L{Who attempts to use extra capabilities as hostile})
     ;; Knife at home being normal vs abnormal and threat detection (food pre-cut)
     ;; Fire at home being normal vs abnormal and autosprinkler (candles, cigarettes)
     ($slide @list{Full Abstraction}
        @L{Strong Separation of concerns} ;; blind quantifiers, parametric polymorphism
        @L{App developer not allowed to see how data is persisted}
        @L{Persistence Implementer not allowed to see data}
        @L{User or Admin can configure service and providers})
     ($slide @list{Storage Providers}
        @L{All data encrypted with use-once salt} ;; regular disk encryption: two-time pad
        @L{Add/Remove Provider... independent worldwide replicas}
        @L{Management layer handles RAID replication}
        @L{Financial layer plans (un)subscriptions}))
    ($section "Programming Model for Orthogonal Persistence"
     $plan-slide
     ($slide @list{Manual Persistence Sucks}
        @L{PL: Everything Transient—except complex storage protocols}
        @L{DB: Everything Table—except Queues, Procedures...}
        @L{Everyone: Processes are inherently transient}
        @L{No eff* way to persist dynamic code, continuations})
     ($slide @list{Transactions are Bad}
        @L{Require Global Knowledge}
        @L{Don't Compose}
        @L{Always too small or too big}
        @L{Low Level Concept})
     ($slide @list{Anti-Transactions are Good}
        @L{“Don't transact here”, a.k.a. Critical Section}
        @L{Section itself small, code that uses it large}
        @L{Local Knowledge Only, Composable}
        @L{High-Level Concept, already used everywhere})
     ($slide @list{Don’t Commit, Synchronize}
        @L{Local constraint, automatically fulfilled}
        @L{Always persist private data before to publish transaction}
        @L{Just like memory barriers in modern processors}
        @L{High-Level Concept, already used everywhere})
     ($slide @list{Databases are Bad}
        @L{Package deal persistence w/ data & execution model} ;; relational, speculative. You don't program in APL.
        @L{Database come with really bad PL} ;; pgsql, oracle sql, etc.
        @L{Databases themselves don't persist} ;; identity tied to a physical/logical machine, a software vendor, etc.
        @L{Databases don't (de)compose}) ;; federating databases: ouch. Adding backends: ouch.
     ($slide @list{Domains are Good}
        @L{Works with your data model, execution model, PL}
        @L{Identity not tied to a machine, vendor, etc.}
        @L{Dynamically add or remove backends}
        @L{Dynamically join (sub)Domains in consensus}))
    ($section "Decoupling Issues from Persistence"
     $plan-slide
     ($slide @list{Never Save and Copy, Publish and Share}
        @L{Never Save, Tag Stable Versions}
        @L{Never Copy, Fork and Join versions… of Processes} ;; entire workspace in git
        @L{Never Copy, (Un)Publish and Share (Sub)Documents}
        @L{Never Forget, Cache and Remember}) ;; preserve identity not just content
     ($slide @list{Never OOM to death, manage quotas}
        @L{Stop processes & escape to metasystem}
        @L{Adjust: increase quota, GC, fix bug…}
        @L{Kill maybe, but also Restart} ;; either way, user-level PCLSRing
        @L{Meta: Predict usage, anticipate increase})
     ($slide @list{Never Migrate, Just Upgrade Schema or Change Backends}
        @L{Schema Upgrade, Change Backends are expected}
        @L{Schema Upgrade built into PL, testing}
        @L{Always persist private data before to publish transaction}
        @L{Schema Upgrade is always eventually expected.})
     ($slide @list{Never Corrupt, Maintain}
        @L{No Corruption-prone yet Static language}
        @L{Low-level or transient code in isolated VMs within Domain}
        @L{Live systems to cultivate, not dead programs to throw away}
        @L{Debug using reflection within version forks}))
    ($section "Conclusion: Consequences of Orthogonal Persistence"
     $plan-slide
     ($slide @list{Different Software Architecture}
        @L{Reflection, Capabilities, Versioning, Upgrade, Consensus…} ;; Different APIs
        @L{Pervasive changes throughout all system} ;; No save, no copy
        @L{Change mindset from PL to System Paradigm}
        @L{Run legacy code in managed VMs}) ;; Legacy
     ($slide @list{Economic Implications: New Markets}
        @L{Documents and Modules, not Apps}
        @L{Competing Blind Storage, not Vertical Mopolonies}
        @L{Local Code, not Spying Ad Servers}
        @L{Admin Delegation, under User Control})
     ($slide @list{Social Implications: Better Division of Labor}
        @L{Users: Empowered, Refocused—Can Delegate Admin}
        @L{Developers: Less Repetition, More Specialization}
        @L{Providers: Breaking Vertical Monopolies}
        @L{Defenders: Better Control on Smaller Attack Surfaces})
     ($slide @list{Thank You!}
        @Li{Model: @Url{https://github.com/mighty-gerbils/gerbil-persist/blob/master/persist.md}}
        @Li{Looking for funding…}
        @Li{X: @Url{https://x.com/ngnghm}  Blog: @Url{https://ngnghm.github.io}}
        @Li{Hire me — or be hired by me! @(~ 5) @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)
