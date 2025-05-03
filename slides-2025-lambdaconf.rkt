#lang at-exp racket @; -*- Scheme -*-
#|
Orthogonal Persistence, the Model

Slides for presentation at LambdaConf 2025 in Estes Park, Colorado, 2025-05-12

To compile it, use:
  mkdir -p run
  ln -s ../nada-poof/util ./
  ln -s ../../nada-poof/resources run/
  ln -s ../pics run/
  racket slides-2025-lambdaconf.rkt > run/slides-2025-lambdaconf.html
where nada-poof is a checkout of
  https://github.com/metareflection/poof

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

(def (haddock x)
  (img src: (format "pics/haddock~a.webp" x) alt: (format "panel ~a" x)
       height: "250vmin" valign: 'top))

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
     ($slide "A Simple Story (1)"
        @;{
             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater,
             is sitting on a chair at a café terrace in San Theodoros.
             Haddock is on the bottom left of the panel;
             we see his back, he is turned slightly towards the right of the panel.
             He is typing on a laptop on the round café table, next to a glass of whisky.
             On the laptop, a white-on-black window on which is displayed "> foo = 42"
             and at the next line "> " aligned under the above ">", followed by a blinking underscore cursor.
             On the top left side of the panel,
             a robber in a grey hoodie is approaching subreptitiously;
             you can't see the robber's face, hidden under his hood.
             In the background, the view from the café is the beach of San Theodoros,
             with a large banner "San Theodoros" greeting people to the beach.
             There are two poor people on the beach.

             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater, is on the bottom left of the panel;
             Haddock is at a café terrace, with a view on the beach of San Theodoros
             on the top left of the panel.
             We see his back, he is turned slightly towards the right of the panel.
             Haddock is half-standing instead of sitting, the round café table is wobbling in front of him,
             and the glass of whisky on it is falling.
             His laptop is now in the hands of the robber in a gray hoodie that hides his face,
             who stole it, and who is running away with it, away from Captain Haddock.
             The robber is on the right of the panel, heading away towards the top right of the panel.
             Captain Haddock, is cursing "Buccaneer!" and other censored curses.
             His left hand is on the table, and his right fist is raised. He has only two arms.
             The speech bubble points at Captain Haddock.
             In the background, the view from the café is the beach of San Theodoros,
             with a large banner "San Theodoros" greeting people to the beach.
             There are two poor people on the beach.

             Generate a comic panel in the genre of Hergé.
             We see Captain Haddock with a unicorn on his sweater, flabbergasted,
             three-quarter facing us, turned slightly towards the right of the panel,
             calling a customer support line.
             Haddock is holding a newspaper, the San Theodoros Times, that sports a headline:
             "Nuevo-Rican missile hits datacenter".
             The voice on the phone, with a speech bubble pointing at the listening part of the phone,
             tells him:
             "Yes, our engineers lost your data. Also everyone else's. Also their lives."

             Generate a comic panel in the genre of Hergé.
             On the bottom left of the panel, Captain Haddock, with a unicorn on his sweater,
             is grumbling, sitting laptopless on a plane from San Theodoros back to Marlinspike.
             He is three quarter face, facing towards the right of the panel.
             On the porthole in front of him, you can see the wing of the plane he's in.
             The picture of Haddock is itself but an insert in a background map
             that shows the plane itinerary.
             The map does not have label, but an arrow that covers the ocean part
             of the travel only, pointing towards Europe, point from (not towards) Paraguay.

             Generate a comic panel in the genre of Hergé.
             A computer technician is on the bottom right of the panel, seen from his back,
             at the bottom of the flight of stairs down the grand portico in front of Marlinspike Hall.
             Captain Haddock on the top left of the panel comes out the door and down the staircase
             towards the technician to the right of the panel,
             his arms open wide and high to effusively greet the technician.
             Haddock says: "Ah my savior!"
             Haddock is holding a glass of whisky, and the whisky is ejected from the glass by Haddock's
             sudden arm movement.
             Nestor the Marlinskpike butler clad in black tailcoat, white shirt, black bowtie,
             no mustache, balding grayhaired, is standing next to Haddock on the staircase nearer the top of the panel.
             Nestor is splashed by the whisky on his face.
             Nestor is startled and tips the platter he was carrying.
             The bottle of whisky on that platter falls.
             The technician, turned towards captain Haddock, carries in his hands a white box
             with a picture of a laptop on it. He offers the box to Captain Haddock.
             The technician declares: "It's preinstalled, just enter your credentials".
             The speech bubble for that points to the technician.

             Generate a comic panel in the genre of Hergé.
             Captain Haddock, with a unicorn on his sweater,
             was sitting on a chair at a terrace in his garden at Marlinspike Hall.
             He is on the bottom left of the panel;
             we see his back, he is turned slightly towards the right of the panel.
             On the table he has a laptop,
             with white-on-black window on which is display "> foo"
             and at the next line "42" aligned under the above ">",
             and yet on the next line, a prompt "> " aligned as well, followed by a blinking underscore cursor.
             Haddock says contentedly: "Yes! All definitions are still there."
             The speech bubble points at Haddock.
             On the top left side of the panel,
             Nestor the Marlinskpike butler is bringing the captain a bottle of whisky and a glass on a platter.
        ;}
        @(make-table (map (lambda (x) (list (map haddock x))) '((1 2 3)))))
     ($slide "A Simple Story (2)"
        @(make-table (map (lambda (x) (list (map haddock x))) '((4 5 6))))

        @;{@L{At a REPL on your laptop, you define @code{foo}}
        @L{You involuntarily donate your laptop to a “youth”}
        @L{Orcs raze your data center}
        @L{Back home, install new laptop, enter credentials}
        @L{Your session is as before, @code{foo} still defined}
        @L{... yet not a single LoC involved backing up data};} ;; Actually, most code is forbidden to even try to back up data.
        )
     ($slide "Persistence vs What?"
        @L{Persistence is @em{when your data survives}}
        @L{BUT! Failure model: @em{what} does it survive?} ;; Compare with the notion of Attack Model wrt Security claims
        @L{Unexpected: power loss, accident, @em{hack}, theft, fire, war}
        @L{Expected: hardware and software end-of-life})
     ($slide @list{Old Tech Made New Again}
        @L{OODB 1980s-now: Lisp, Smalltalk, C++, Java...} ;; Continuations don't persist!
        @L{1980s-1990s: EUMEL, EROS… Funding dried 2000s-2010s.} ;; A few do persist continuations. Grasshopper. Funding dried.
        @L{Today “Durable Execution”. Renewed research.}
        @L{Me: From my good old HP28 to ORMs to StrongBOX })
     ($slide @list{Thesis}
        @L{Orthogonal Persistence (OP) is the Right Thing™}
        @L{Persisting Continuations simplifies everything else}
        @L{Transactions are the wrong abstraction}
        @L{OP completely solves many problems, elevates the rest}))
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
        @L{@code{
          (me (work (client1 client2 client3 hr)) @bri
           @~ (home (cooking kids taxes)) @bri
           @~ (secret (cia kgb mosad)) @bri
           @~ (love (wife lover1 lover2)))
        }})
     ($slide @list{Domains}
        @L{User creates sub-domains}
        @L{Origin Domain Validates Copy Out / Share Out}
        @L{Destination Domain Validates Paste In / Share In}
        @L{Resource Accounting, Storage Providers})
     ($slide @list{Document}
        @L{Edit, Interact at will}
        @L{Always Saved}
        @L{Not always publishable}
        @L{Share some versions with some people, one way or both}))
    ($section "Orthogonal Persistence for Developers"
     $plan-slide
     ($slide @list{Just Do It}
        @L{Variables and @em{Processes} “just” persist, can be shared}
        @L{No more save and restore, copy and send}
        @L{Eliminate 30%-70% of all code — just for variables}
        @L{Processes may no longer fail mid-invariant})
     ($slide @list{Capabilities: Photo App}
        @L{Photo App: save... over net... download extensions} ;; also camera, etc.
        @L{Regular App: will compromise your entire computer}
        @L{Persistent App: can only edit given picture}
        @L{Wider Reflective Capability Architecture})
     ($slide @list{Security Improvement}
        @L{Fewer things to think about} ;; for AIs as well as humans
        @L{Fewer things to get wrong}
        @L{Smaller attack surface}
        @L{Whoever reaches for forbidden capabilities is hostile})
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
        @L{PL: Everything Transient—but via complex storage protocols}
        @L{DB: Everything a Table—but Queues, Procedures...}
        @L{Everyone: Processes are inherently transient}
        @L{No eff* way to persist continuations or dynamic code})
     ($slide @list{Transactions are Bad}
        @L{Require Global Knowledge}
        @L{Don't Compose}
        @L{Always too small or too big}
        @L{Low Level Concept, no one wants to use}
        @L{... Necessary because processes don't persist})
     ($slide @list{Anti-Transactions are Good} ;; Critically requires processes to persist!
        @L{“Don't transact here”, a.k.a. Critical Section}
        @L{Section itself small, code that uses it large}
        @L{Local Knowledge Only, Composable}
        @L{High-Level Concept, already used everywhere}
        @L{... Possible because processes do persist})
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
        @L{Identity not tied to a disk, machine, vendor, etc.}
        @L{Dynamically add or remove backends}
        @L{Dynamically join (sub)Domains in consensus}))
    ($section "Decoupling Issues from Persistence"
     $plan-slide
     ($slide @list{Never Save and Copy, Publish and Share}
        @L{Never Save, Tag Stable Versions}
        @L{Never Copy and Modify, Fork Versions} ;; entire workspace in git
        @L{Never Copy and Send, Publish and Share}
        @L{Never Forget, Cache and Remember}) ;; preserve identity not just content
     ($slide @list{Never get OOM, manage quotas}
        @L{Stop processes & escape to metasystem}
        @L{Adjust and Restart: increase quota, GC, fix bug…} ;; either way, user-level PCLSRing
        @L{Meta: Predict usage, anticipate increase} ;; unlike today, don't lose coupling of live code and data
        @L{Remember correlation of live code and data}) ;; unlike today
     ($slide @list{Never Migrate, Upgrade and Reconfigure}
        @L{All Schema, Backends will eventually get obsolete}
        @L{Schema Upgrade support in PL, testing}
        @L{Reconfigure backends, no lock-in to hw, sw, vendor}
        @L{Normal operations, not million-dollar crises})
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
        @L{Local Code, not Spying Advertising App Servers}
        @L{Competing Blind Storage, not Vertical Monopolies}
        @L{Admins answer to Users, not to App Vendors})
     ($slide @list{Social Implications: Better Division of Labor}
        @L{Users: Empowered, Refocused—Can Delegate Admin}
        @L{Developers: Less Repetition, More Specialization}
        @L{Providers: Breaking Vertical Monopolies}
        @L{Defenders: Better Control on Smaller Attack Surfaces})
     ($slide @list{Orthogonal Persistence's Time Has Come!}
        @L{Whoever makes it usable first will redefine the industry}
        @L{Model: @Url{https://github.com/mighty-gerbils/gerbil-persist}}
        @L{X: @Url{https://x.com/ngnghm} @Li{Blog: @Url{https://ngnghm.github.io}}}
        @L{Fund me — or be hired by me! @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)


#|
TODO:
make it clear in the story it's for users, not just for developers
show examples of persisting data with ChatGPT generated code in Python vs in TScheme
Using AI in Python without strongbox is a huge security risk
Using AI in TScheme with strongbox is A-OK

Focus 100% on business logic

MV: on every slide, have the first part (3/4) accessible to everybody, including end-users.
MV: only last 1/4 of the slide for the programmer.
...
and what does the programmer have to do to support it?
...
Traditional system...
vs
Our system...

We can be the Wright Brothers (or Henry Ford) of Orthogonal Persistence.
They were not the only ones trying to get fly , but they
--- the first ones to get it off the ground for real.
We are the times when SOMEONE will get it right.
You can be the ones who get the magic ticket.
Whoever gets it right will have a lot of control on the future.

|#
