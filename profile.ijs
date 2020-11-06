NB. J profile/prelude for emacs
ARGV_z_ =: < 'emacs'
jpathsep_z_=: '/'&(('\' I.@:= ])})
bin =. BINPATH_z_ =. '/.guix-profile/bin',~home=. 2!:5'HOME'
install=. '/Applications/j901'
'addons system tools'=. install&, &.> '/addons';'/system';'/tools'
user=. home,userx=. '/j902-user'
'break config snap temp'=. user&, &.> '/break';'/config';'/snap';'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'
SystemFolders_j_=: ids,.jpathsep@".&.>ids
md=. 3 : 0
if. -.#1!:0 }:a=.y,'/' do. for_n. I. a='/' do. 1!:5 :: [ <n{.a end. end.
)
md &.> (user,'/projects');break;config;snap;temp NB. should handle in emacs?
0!:0 <jpathsep (4!:55 (;:'userx ids md'), ids)]system,'/util/boot.ijs'
require 'viewmat plot stats/bonsai'
VISIBLE_jviewmat_ =: 0 NB. suppress viewmat from trying to open file itself

NB. letters
az =: a.{~97+i.26
AZ =: a.{~65+i.26
a09 =: a.{~48+i.10

NB. pala=: 0.5 0.5 0.5
NB. palb=: 0.5 0.5 0.5
NB. palc=: 1.0 1.0 1.0
NB. pald =: 0 0.33 0.67
NB. 
NB. plt =: 3 : '<. 255 * 1 <. 0 >. pala + palb * 2 o. 2p1 * pald + palc * y'
NB. 
NB. palette=: plt"0 (255 %~ i. _256)

NB. **********************
NB. * org mode utilities *
NB. **********************
link =: '[[file:'&, @ ,&']]'
linkf =: 4 : 0
  NB. x linkf y: write bytes y to file x. intended for org mode 
  y 1!:2 < x
  link x
)
linki =: 4 : 0
  NB. x linki y: write image y to file x. intended for org mode
  require 'ide/qt/qtlib'
  y writeimg_jqtide_ x
  link x
)
linkv =: 4 : 0
  NB. x linkv y: viewmat matrix y to file x. intended for org mode
  v =. VISIBLE_jviewmat_
  VISIBLE_jviewmat_ =: 0
  viewmat y
  2!:1 'cp ',(jpath '~user/temp/viewmat.png'),' ',x
  VISIBLE_jviewmat_ =: v
  link x
)
linkadv =: 1 : 0
  NB. x linkv y: viewmat matrix y to file x. intended for org mode
  v =. VISIBLE_jviewmat_
  VISIBLE_jviewmat_ =: 0
  u viewmat y
  2!:1 'cp ',(jpath '~user/temp/viewmat.png'),' ',x
  VISIBLE_jviewmat_ =: v
  link x
)
linkvbw =: 4 : 0
  NB. x linkv y: black/white viewmat matrix y to file x. intended for
  NB. org mode
  v =. VISIBLE_jviewmat_
  VISIBLE_jviewmat_ =: 0
  (0 0 0,:255 255 255) viewmat y
  2!:1 'cp ',(jpath '~user/temp/viewmat.png'),' ',x
  VISIBLE_jviewmat_ =: v
  link x
)
orgstrip =: 4 : 0
NB. input chars x, output chars y.
NB. strip input from output from having run '0!:1'.
x=. <;._2 x [ y=. <;._2 y [ o=. ''
while. #x do. lx=. >{.x [ ly=. >{.y
  if. -. (ly-:'   ',lx) +. ly-:lx
   do.   o =. o,ly,LF [ y=. }.y
   else. x=. }.x [ y=. }.y end.
end. o
)
