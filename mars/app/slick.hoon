::  slick: no step on slick
::
::  ▐█ ▀█▪▪     ▪     █▌▄▌▪  ·•▄
::
::  ▄▄██▛▟▛▟▛▟▛▟▛▟██▙█    ▄▟▛ ▟█▛ ▟█▛    ▖ ▗ ▘ ▙ ▚ ▛ ▜ ▝ ▞ ▟ ▀
::
:: l/r   ▖    ▗    ▘    ▙    ▚    ▛    ▜    ▝    ▞    ▟
::  ▖   ▖▖   ▖▗   ▖▘   ▖▙   ▖▚   ▖▛   ▖▜   ▖▝   ▖▞   ▖▟
::  ▗   ▗▖   ▗▗   ▗▘   ▗▙   ▗▚   ▗▛   ▗▜   ▗▝   ▗▞   ▗▟
::  ▘   ▘▖   ▘▗   ▘▘   ▘▙   ▘▚   ▘▛   ▘▜   ▘▝   ▘▞   ▘▟
::  ▙   ▙▖   ▙▗   ▙▘   ▙▙   ▙▚   ▙▛   ▙▜   ▙▝   ▙▞   ▙▟
::  ▚   ▚▖   ▚▗   ▚▘   ▚▙   ▚▚   ▚▛   ▚▜   ▚▝   ▚▞   ▚▟
::  ▛   ▛▖   ▛▗   ▛▘   ▛▙   ▛▚   ▛▛   ▛▜   ▛▝   ▛▞   ▛▟
::  ▜   ▜▖   ▜▗   ▜▘   ▜▙   ▜▚   ▜▛   ▜▜   ▜▝   ▜▞   ▜▟
::  ▝   ▝▖   ▝▗   ▝▘   ▝▙   ▝▚   ▝▛   ▝▜   ▝▝   ▝▞   ▝▟
::  ▞   ▞▖   ▞▗   ▞▘   ▞▙   ▞▚   ▞▛   ▞▜   ▞▝   ▞▞   ▞▟
::  ▟   ▟▖   ▟▗   ▟▘   ▟▙   ▟▚   ▟▛   ▟▜   ▟▝   ▟▞   ▟▟
::
/+  *etui, dbug, verb, default-agent
::
|%
+$  state
  $:  sesh=@ta
      area=size  ::  play area at [1 2], local coordinate system (x = x/2)
      live=$~(%ceased ?(%living %paused %ceased))
      slick=(list spot)  ::  head-first
      face=?(%u %d %l %r)  ::TODO  store current direction?
      food=spot
      time=@ud
      next=(unit @da)
  ==
::
+$  fect
  $%  [%start when=@da]  ::  start frame timer
      [%pause when=@da]  ::  stop frame timer
      [%alert ~]         ::  beep!
  ==
::
+$  part  ::  direction from tail to head
  $~  %h
  $?  %h                   ::  head
      %o                   ::  nom
      %f                   ::  fat
      %l  %r  %u  %d       ::  straight
      [from=part to=part]  ::  turn
  ==
::
++  config
  |%
  ++  start-length  5
  ++  speed         (div ~s1 2)
  ++  gain          (div ~s1 60)
  ++  peak          20
  --
::
++  input
  |=  [=state =belt:dill now=@da eny=@]
  ^-  (quip fect _state)
  ?-  live.state
    %ceased  =/  =^state  (init sesh.state area.state now eny)
             [[%start (need next.state)]~ state]
    %paused  =/  =^state  state(live %living, next `(add now speed:config))
             [[%start (need next.state)]~ state]
  ::
      %living
    ?+  belt  [~ state]
      %k        :-  [%pause (need next.state)]~
                state(live %ceased, next ~)
      %t        [[%start (need next.state)]~ state]
      %p        :-  [%pause (need next.state)]~
                state(live %paused, next ~)
      [%aro *]  ?>  ?=([* * *] slick.state)
                ?:  ?&  !=(i.t i):slick.state
                        =(p.belt (direction i.t.slick.state i.slick.state))
                    ==
                  [~ state]
                [~ state(face p.belt)]
    ==
  ==
::  +init: start a new game
::
++  init
  |=  [sesh=@ta area=size now=@da eny=@]
  ^-  state
  :: =.  area  (min:co area [20 20])
  =;  =state
    =.  food.state  (feed area slick.state eny)
    state
  :*  sesh
      area
      %living
      (flop (turn (gulf 0 (dec start-length:config)) (late (div h.area 2))))
      %r
      *spot
      0
      `(add now speed:config)
  ==
::  +step: core game loop
::
::    moves the snake forward in the last chosen direction.
::    "moving" here happens by adding a new head, and cutting off the tail.
::    if we move onto the location of food, add a new head twice,
::    once the food moves down into the tail, it'll take an extra step to
::    get snipped off, extending the snake.
::
++  step
  |=  [state now=@da eny=@]
  ^-  (quip fect state)
  =*  state  +<-
  =/  move=(unit spot)
    =+  (snag 0 slick)
    ?-  face
      %l  ?:(=(0 x) ~ `[(dec x) y])
      %u  ?:(=(0 y) ~ `[x (dec y)])
      %r  ?:((gte +(x) w.area) ~ `[+(x) y])
      %d  ?:((gte +(y) h.area) ~ `[x +(y)])
    ==
  ?~  move
    (kill state)
  ?:  (lien slick (cury test u.move))
    (kill state)
  =.  slick  [u.move (snip slick)]
  =?  slick  =(food u.move)  [u.move slick]
  ?:  =((lent slick) (mul [w h]:area))
    (kill state)
  =?  food  =(food u.move)  (feed area slick eny)
  =.  time  +(time)
  =.  next
    :-  ~
    %+  add  now
    %+  sub  speed:config
    %+  mul  gain:config
    %+  min  peak:config
    %+  sub  (lent slick)
    start-length:config
  [[%start (need next)]~ state]
::
++  feed
  |=  [size slick=(list spot) eny=@]
  ^-  spot
  =/  r  ~(. og eny)
  =/  i  (snag 0 slick)
  =/  l  (mul 2 (lent slick))
  |-
  ::TODO  there's probably some cut-off where just enumerating all the empty
  ::      tiles and picking from those is faster...
  =^  s=spot  r
    =^  x  r  (rads:r (dec w))
    =^  y  r  (rads:r (dec h))
    [[x y] r]
  ?:  (gth (add (dif:co s i)) l)  $  ::  spawn close to slick initially, for demo
  ?:((lien slick (cury test s)) $ s)
::
++  kill
  |=  =state
  ^-  (quip fect _state)
  :-  ~[[%pause (need next.state)] [%alert ~]]
  state(live %ceased, next ~)
::
::
++  draw
  |=  state
  ^-  card:agent:gall
  =;  =blit:dill
    [%give %fact [/dill/[sesh]]~ %dill-blit !>(blit)]
  :-  %mor
  =-  (snoc - [%hop (mul 2 w.area) 0])
  %+  weld  (frame area)
  ?-  live
    %ceased  [[%hop 1 0] [%put `(list @)`"ur ded lol, hit key to restart"] ~]
    %paused  [[%hop 1 0] [%put `(list @)`"paused, hit key to resume"] ~]
    %living  ^-  (list blit:dill)
             :+  [%hop 1 0]
               =/  score=@ud  (sub (lent slick) start-length:config)
               [%put `(list @)`"score: {(scow %ud score)}"]
             :+  (cure:(play-zone area) (mul 2 x.food) y.food)  [%put `(list @)`"<>"]
             (draw-slick area (flat-slick (read-slick slick)) face)
  ==
::
++  frame
  |=  =size
  ^-  (list blit:dill)
  ::TODO  dedupe with +play-zone
  =.  w.size  (add 3 (mul w.size 2))
  =.  h.size  (add h.size 2)
  :~  [%clr ~] ::~(wipe zi [0 0] w.size (add h.size 3))
    ::
      %-  ~(line zi [1 1] size)
      [*stye ~ (taft '~') (taft '⸾') (taft '+')]
  ==
::
++  read-slick
  |=  slick=(list spot)
  ^-  (list [spot part])
  =/  las=?(%l %r %u %d ~)  ~
  |-
  ?~  slick  ~
  ?~  t.slick
    ?<  ?=(~ las)
    [[i.slick las] ~]
  =*  h  i.slick
  =*  t  i.t.slick
  ?:  =(h t)
    ::  draw food-in-the-snake
    =-  [[h -] $(slick t.slick)]
    ?:(=(~ las) %o %f)
  =/  d  (direction h t)
  :_  $(slick t.slick, las d)
  :-  h
  ^-  part
  ?:  ?=(~ las)  %h
  ?:  =(d las)   d
  [d las]
::
++  flat-slick
  |=  slick=(list [=spot =part])
  ^-  (list [spot (list part)])
  ?~  slick  ~
  =/  =spot  [spot.i.slick]
  =/  parts  `(list part)`[part.i.slick]~
  |-
  ?~  t.slick  [spot parts]~
  ?.  =(y.spot y.spot.i.t.slick)
    [[spot parts] ^$(slick t.slick)]
  =.  parts
    ?:  (lth x.spot x.spot.i.t.slick)
      (snoc parts part.i.t.slick)
    ?:  (gth x.spot x.spot.i.t.slick)
      [part.i.t.slick parts]
    parts
  =.  x.spot
    (min x.spot x.spot.i.t.slick)
  $(t.slick t.t.slick)
::
++  play-zone  ::TODO  is this actually better?
  |=  =size
  ~(. zi [2 2] (mul 2 w.size) (add h.size 2))
::
++  draw-slick
  |=  [=size slick=(list [spot (list part)]) face=?(%l %r %u %d)]
  ^-  (list blit:dill)
  %-  zing
  %+  turn  slick
  |=  [=spot pas=(list part)]
  ^-  (list blit:dill)
  ::TODO  would this transform be easier if we had a +play-zone arm?
  =.  x.spot  (mul 2 x.spot)
  =;  =tour
    [(cure:(play-zone size) spot) [%put tour] ~]
  %-  zing
  ^-  (list tour)
  %+  turn  pas
  |=  p=part
  %-  tuba
  ?+  p  !!
    %h       ?-(face ?(%l %d) "(:", ?(%u %r) ":)")
    %o       ?-(face ?(%l %d) "O:", ?(%u %r) ":O")
    %f       "▓▓"
    %l       "██"
    %r       "██"
    %u       "██"
    %d       "██"
    [%l %u]  "▀█"
    [%l %d]  "▄█"
    [%r %u]  "█▀"
    [%r %d]  "█▄"
    [%u %l]  "█▄"
    [%u %r]  "▄█"
    [%d %l]  "█▀"
    [%d %r]  "▀█"
  ==
::
++  direction  ::  angle of a relative to b
  |=  [a=spot b=spot]
  ^-  ?(%l %r %u %d)
  ?:  =(x.a x.b)
    ?:  (gth y.a y.b)  %d
    ?:  (lth y.a y.b)  %u
    !!
  ?:  =(y.a y.b)
    ?:  (gth x.a x.b)  %r
    ?:  (lth x.a x.b)  %l
    !!
  !!
::
++  execute
  |=  [sesh=@ta =fect]
  ^-  card:agent:gall
  ?-  -.fect
    %start  [%pass /timestep %arvo %b %wait when.fect]
    %pause  [%pass /timestep %arvo %b %rest when.fect]
    %alert  [%give %fact [/dill/[sesh]]~ %dill-blit !>([%bel ~])]
  ==
--
::
=|  state
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this(area [40 40])]
++  on-save  !>(state)
::
++  on-load
  |=  ole=vase
  =.  state  !<(^state ole)
  =.  live  %ceased
  [[(draw state)]~ this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%dill-poke mark)  (on-poke:def mark vase)
  =+  !<([ses=@ta belt=dill-belt:dill] vase)
  =.  sesh  ses
  ?:  ?=([%yow *] belt)
    ~&  %slick-yow
    [~ this]
  ?:  ?=([%cru *] belt)
    ~&  %slick-cru
    [~ this]
  ?:  ?=([%hey *] belt)
    [[(draw state)]~ this]
  ?:  ?=([%rez *] belt)
    ?.  ?=(%ceased live.state)
      [~ this]
    =.  area  [(sub (div p.belt 2) 2) (sub q.belt 3)]
    [[(draw state)]~ this]
  =^  fex  state  (input state belt [now eny]:bowl)
  :: =^  fez  state  (step state [now eny]:bowl)
  [[(draw state) (turn fex (cury execute sesh))] this]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?.  ?=([%dill @ ~] path)  (on-watch:def path)
  =.  sesh  i.t.path
  ::TODO  don't do this and await hey instead?
  [[(draw state)]~ this]
::
++  on-leave
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?.  ?=([%dill @ ~] path)  (on-leave:def path)
  [~ this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ?.  =(/timestep wire)
    (on-arvo:def wire sign-arvo)
  =^  fex  state  (step state [now eny]:bowl)
  [[(draw state) (turn fex (cury execute sesh))] this]
::
++  on-agent  on-agent:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
