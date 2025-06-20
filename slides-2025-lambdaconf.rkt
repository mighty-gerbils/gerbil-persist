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
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 140%"
            @b{Orthogonal Persistence: @br the Model}]
       @(br clear: 'all)
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
             you can’t see the robber’s face, hidden under his hood.
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
             "Yes, our engineers lost your data. Also everyone else’s. Also their lives."

             Generate a comic panel in the genre of Hergé.
             On the bottom left of the panel, Captain Haddock, with a unicorn on his sweater,
             is grumbling, sitting laptopless on a plane from San Theodoros back to Marlinspike.
             He is three quarter face, facing towards the right of the panel.
             On the porthole in front of him, you can see the wing of the plane he’s in.
             The picture of Haddock is itself but an insert in a background map
             that shows the plane itinerary.
             The map does not have label, but an arrow that covers the ocean part
             of the travel only, pointing towards Europe, point from (not towards) Paraguay.

             Generate a comic panel in the genre of Hergé.
             At the top left of the panel is the grand portico of Marlinspike Hall,
             with a staircase going down from it towards the right.
             At the bottom right of the panel is the bottom of the staircase, at street level.
             A computer technician is on the bottom right of the panel, seen from his back,
             at the bottom of the staircase.
             Captain Haddock, with a unicorn on his sweater,
             is on the top of left of the panel facing right and walking down the staircase,
             and whisky is ejected from the glass in his hand as as he suddenly opens his arms wide and high
             to effusively greet the technician.
             The technician, to the right of the panel, turned towards captain Haddock,
             carries in his hands a white box with a picture of a laptop on it.
             He offers the box to Captain Haddock.
             A speech bubble from Haddock says: "Ah my savior!"
             Another speech bubble from the technician declares: "It’s preinstalled, just enter your credentials".

             Nestor the Marlinskpike butler clad in black tailcoat, white shirt, black bowtie,
             no mustache, balding grayhaired, is standing next to Haddock on the staircase nearer the top of the panel.
             Nestor is splashed by the whisky on his face.
             Nestor is startled and tips the platter he was carrying.
             The bottle of whisky on that platter falls.

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
        @(make-table (map (lambda (x) (list (map haddock x))) '((4 5 6)))))
        @;{
        @L{At a REPL on your laptop, you define @code{foo}}
        @L{You involuntarily donate your laptop to a “youth”}
        @L{Orcs raze your data center}
        @L{Back home, install new laptop, enter credentials}
        @L{Your session is as before, @code{foo} still defined}
        @C{… yet not a single LoC involved backing up data} ;; Actually, most code is forbidden to even try to back up data.
        ;}
     ($slide "Persistence"
        @L{Persistence is @em{when your data survives}}
        @L{Failure model: @em{what} does it survive?} ;; Compare with the notion of Attack Model wrt Security claims
        @L{Unexpected: power loss, accident, @em{hack}, theft, fire, war}
        @L{Expected: hardware and software end-of-life})
     ($slide "Orthogonal"
        @L{Orthogonal: independent from what the program specifies}
        @L{The system, not the application, persists data}
        @L{System programmers do it once for all of everyone’s data}
        @L{Rather than reinvented differently by every application})
     ($slide "Old Tech Made New Again"
        @L{OODB 1980s-now: Lisp, Smalltalk, C++, Java…} ;; Continuations don’t persist!
        @L{1980s-1990s: EUMEL, Grasshopper, EROS…}
        @L{Funding dried 2000s-2010s} ;; A few do persist continuations. Grasshopper. Funding dried.
        @L{Today “Durable Execution”. Renewed research.}
        @L{Me: From my good old HP28 to ORMs to this vision}) ;; All the old technology with a new twist to make it usable by regular people
     ($slide "Thesis (Technical)"
        @L{Orthogonal Persistence (OP) is the Right Thing™}
        @L{Persisting Processes simplifies everything else}
        @L{Database Transactions are the wrong Abstraction}
        @L{OP completely solves many problems, elevates the rest})
     ($slide "Thesis (Socio-Economical)"
        @L{Orthogonal Persistence is Economically Inevitable}
        @L{OP will drastically change Software Architecture}
        @L{OP will disrupt existing Business Models}
        @L{OP will improve Division of Labor}
        @C{… a Great Business Opportunity TODAY}))
    ($section "Today: Fractal Transience"
     $plan-slide
     ($slide "Disappointing Users’ Natural Assumption"
        @L{My mom’s book database}
        @L{Who never lost data to no/bad/lost backups?}
        @L{C-x C-s in my fingers} ;; persistence automated... in wetware... but not if computer stolen
        @L{“If I didn’t mean for the computer to remember it, @br I wouldn’t type it”}
        @C{… But don’t modern Apps autopersist data?})
     ($slide "Vendor Lock-In Persistence"
        @L{Easy case only: won’t sync across devices, accounts…}
        @L{Product Cancellation, Change of Ownership, Bankruptcy}
        @L{Forced Code Upgrade or Lack Thereof}
        @L{Data Hostage: Pay Bills & Pray Vendor Healthy})
     ($slide "Developers: Hard Mode"
        @L{Computers, processes, etc., start empty}
        @L{Explicitly save and restore all data}
        @L{Storage Zoo: FS, DB, Web, DVCS…}
        @L{Fake it the hard way, and leave mess to Admin})
     ($slide "Hard Problems"
        @L{Managing Many Out-of-Sync Copies}
        @L{Fragile Backups}
        @L{Schema Upgrades}
        @L{Merging or Splitting Data})
     ($slide "What If...?"
        @L{Persistence was the default}
        @L{No more vendor lock-in}
        @L{Developers don’t have to care... except specialists}
        @L{Hard problems are made easier somehow?}))
    ($section "Orthogonal Persistence for Users"
     $plan-slide
     ($slide "It’s Just There"
        @L{Data “just” persists - Running Processes, too}
        @L{Code runs in ambient “domain” - persistent VM/container}
        @L{Interaction (e.g. Editing Document), not “Apps”}
        @L{Caveat: Subscribe to Backup Storage Provider}) ;; Admin role, can be delegated to accountable party
     ($slide "Example Domains"
        @L{@code{
          (me (work (client1 client2 client3 hr)) @bri
           @~ (home (accounting kids cooking diy)) @bri
           @~ (secret (cia kgb mossad)) @bri
           @~ (love (wife lover1 lover2)))}})
     ($slide "Domains"
        @L{User Creates Sub-Domains}
        @L{Origin Domain Validates Copy Out / Share Out}
        @L{Destination Domain Validates Paste In / Share In}
        @L{Resource Accounting, Backup Configuration})
     ($slide "Activities"
        @L{Edit Document, Interact at will}
        @L{Always Saved, auto retention of old version}
        @L{Not always publishable}
        @L{Share some versions with some people, one way or both}))
    ($section "Orthogonal Persistence for Developers"
     $plan-slide
     ($slide "Just Do It"
        @L{Variables and @em{Processes} “just” persist, can be shared} ;; not "Unix" processes...
        @L{No more save and restore, copy and send}
        @L{Eliminate 30%-70% of all code — just for variables}
        @L{Processes may no longer fail mid-invariant})
     ($slide "Capabilities"
        @L{Photo App: save... over net... download extensions} ;; also camera, etc.
        @L{Transient System: App can compromise everything}
        @L{Persistent System: App can only edit given picture}
        @L{Wider Reflective Capability Architecture})
     ($slide "Security Improvement"
        @L{Narrower target on which to focus limited resources}
        @L{Fewer things to get wrong}
        @L{Smaller attack surface}
        @L{Flag whoever reaches for forbidden capabilities as hostile}
        @L{... whether code is written by humans or AI!})
     ;; Knife at home being normal vs abnormal and threat detection (food pre-cut)
     ;; Fire at home being normal vs abnormal and autosprinkler (candles, cigarettes)
     ($slide "Full Abstraction"
        @L{Strong Separation of concerns} ;; blind quantifiers, parametric polymorphism
        @L{App developer not allowed to see how data is persisted}
        @L{Persistence Implementer not allowed to see data}
        @L{User or Admin can configure service and providers})
     ($slide "Backup Configuration"
        @L{All data encrypted with use-once salt} ;; regular disk encryption: two-time pad
        @L{Add/Remove Provider... independent worldwide replicas}
        @L{Management layer handles RAID replication}
        @L{Financial layer plans (un)subscriptions}))
    ($section "Programming Model for Fractal Transience"
     $plan-slide
     ($slide "Transient Programming Languages Sucks"
        @L{PL: Everything is Transient—especially processes}
        @L{Complex storage protocols}
        @L{Atomicity super hard}
        @L{Persistence relies on wetware conventions})
     ($slide "Databases are Bad (1)"
        @L{Atomicity, but very bad Package Deal}
        @L{Terrible programming languages} ;; pgsql, oracle sql, etc.
        @L{Horrible Data Model: Everything is a Table, except…} ;; Queues, Procedures. Very few want to program with tables. They use Spreadsheets or APL. Sometimes only one table. Store JSON.
        @L{Horrible Evaluation Model: client/server split, speculation…}) ;; split between "client" and "server". Speculation to increase server number but
     ($slide "Databases are Bad (2)"
        @L{No eff* way to persist processes or user-defined code}
        @L{Databases themselves don’t persist} ;; identity tied to a physical/logical machine, a software vendor, etc.
        @L{Databases don’t (de)compose} ;; federating databases: ouch. Adding backends: ouch.
        @L{Rigid Capabilities, Rigid Schema})
     ($slide "System Myopia"
        @C{A programming language without good data persistence is @br
           for toy computations about irrelevant data.}
        @C{A database without a good programming language is @br
           for toy data not part of any relevant computation.}))
    ($section "Programming Model for Orthogonal Persistence"
     $plan-slide
     ($slide "Transactions are Bad"
        @L{Require Global Knowledge}
        @L{Don’t Compose}
        @L{Always too small or too big}
        @L{Low Level Concept, no one wants to use}
        @L{... Necessary because processes don’t persist})
     ($slide "Anti-Transactions are Good" ;; Critically requires processes to persist!
        @L{“Don’t transact here”, a.k.a. Critical Section}
        @L{Section itself small, code that uses it large}
        @L{Local Knowledge Only, Composable}
        @L{High-Level Concept, already used everywhere}
        @L{... Possible because processes do persist})
     ($slide "Don’t Commit, Synchronize"
        @L{Local constraint, easily automated}
        @L{Always persist transaction before to send}
        @L{Just like memory barriers in modern processors}
        @L{High-Level Concept, already used everywhere})
     ($slide "Domains are Good"
        @L{Works with your data model, your execution model, your PL}
        @L{Identity not tied to a disk, machine, vendor, etc.} ;; missing concept: reified handling of identity. Address, URL
        @L{Dynamically add or remove backends}
        @L{Dynamically join (sub)Domains in consensus}))
    ($section "Decoupling Issues from Persistence"
     $plan-slide
     ($slide "Never Save and Copy, Publish and Share"
        @L{Never Save, Tag Stable Versions}
        @L{Never Copy and Modify, Fork Versions} ;; entire workspace in git
        @L{Never Copy and Send, Publish and Share}
        @L{Never Forget, Cache and Remember}) ;; preserve identity not just content
     ($slide "Never get OOM, manage quotas"
        @L{Stop processes & escape to metasystem}
        @L{Adjust and Restart: increase quota, GC, fix bug…} ;; either way, user-level PCLSRing
        @L{Meta: Predict usage, anticipate increase} ;; unlike today, don’t lose coupling of live code and data
        @L{Remember correlation of live code and data}) ;; unlike today
     ($slide "Never Migrate, Upgrade and Reconfigure"
        @L{All Schema, Backends will eventually get obsolete}
        @L{Schema Upgrade support in PL, testing}
        @L{Reconfigure backends, no lock-in to hw, sw, vendor}
        @L{Normal operations, not million-dollar crises})
     ($slide "Never Corrupt, Maintain"
        @L{No Corruption-prone yet Static language}
        @L{Low-level or transient code in isolated VMs within Domain}
        @L{Live systems to cultivate, not dead programs to throw away}
        @L{Debug using reflection within version forks}))
    ($section "Conclusion: Consequences of Orthogonal Persistence"
     $plan-slide
     ($slide "Thesis (Recap)"
        @L{Orthogonal Persistence (OP) is the Right Thing™}
        @L{Persisting Continuations simplifies everything else}
        @L{Transactions are the wrong abstraction}
        @L{OP completely solves many problems, elevates the rest})
     ($slide "Different Software Architecture"
        @L{Reflection, Capabilities, Versioning, Upgrade, Consensus…} ;; Different APIs
        @L{Pervasive changes throughout all system} ;; No save, no copy
        @L{Change mindset from PL to System Paradigm}
        @L{Run legacy code in managed VMs}) ;; Legacy
     ($slide "Economic Implications: New Markets"
        @L{Documents and Modules, not Apps}
        @L{Local Code, not Spying Advertising App Servers}
        @L{Competing Blind Storage, not Vertical Monopolies}
        @L{Admins answer to Users, not to App Vendors})
     ($slide "Social Implications: Better Division of Labor"
        @L{Users: Empowered, Refocused—Can Delegate Admin}
        @L{Developers: Less Repetition, More Specialization}
        @L{Providers: Breaking Vertical Monopolies}
        @L{Defenders: Better Control on Smaller Attack Surfaces})
     ($slide "Orthogonal Persistence’s Time Has Come!"
        @L{Whoever makes it usable first will redefine the industry}
        @L{Model: @Url{https://github.com/mighty-gerbils/gerbil-persist}}
        @L{X: @Url{https://x.com/ngnghm} @br Blog: @Url{https://ngnghm.github.io}}
        @C{Opportunities: @code{<fare@"@"mukn.com>}})))) ;; Investment & Partnership Opportunities, soon I hope job opportunities

(reveal-doc doc)


#|
MV:
Make it clear in the story it’s for users, not just for developers
Show examples of persisting data with ChatGPT generated code in Python vs in TScheme
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

DN: No libertarian jokes

DN: What kind of thign is this?
- OP is an old context
- New take on OP

DN: Backups are the hardest thing to incentivize:
it’s the kind of encrypted data that users generate and nobody, not even themselves, usually need.
Requires protocol wherein the client regularly polls the data.
F: decentralized is better if possible, but not necessary for persistence

DN: long-standing "persistent" issue that memory, background storage and network have very different speed and latency.
Some chinese companies have developed locally persistent memory as fast as RAM.
This and very fast network means you can "just" persist everything all the time in a low-level way
without improving the architecture.

DN: General intelligence comes from three things together: minds, literacy and markets.
Local persistence is sort of solved. However, over time, entropy increases, lots of tabs are opened.
Restarting is nearly necessary. Blank slates are necessary. Life and death are necessary for evolution.
F: Good problem to have. Problem elevated / revealed by solving

JE: Make the claim about different software architecture way earlier, business model.

JE: Idea of domain central to me, but what is it? Nail it down clearer.
    Filesystem directory? Database? Unix user? VM?

JE: "Databases are bad" -- central argument. Gonna get some pushback. Be more hedged and humble.
   They have evolved on specialization. Deployment.
   Challenge: we have to beat databases at what they do. Acknowledge problems.

JE: Critical sections vs transactions, etc. What happens to read consistency? Probably too technical for this talk.

JE: Be humble about what transactions are good at. Research problem. Theoretical solution.

JE: More impact if I back off from things that will trigger people.

JE: What is the business model for the new software architecture?

JE: What is the vulnerability model of SaaS products? Users are not in control.

JE: "Fund me" => my DMs are open.

JE: replace lisp (me (work ...)) with a tree diagram

EA: Fewer slides.

EA: Why orthogonal?

Persistence becomes a system service. Just like memory management before...

|#
