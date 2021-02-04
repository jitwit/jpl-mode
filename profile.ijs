NB. J profile/prelude for emacs
ARGV_z_ =: < 'emacs'
jpathsep_z_=: '/'&(('\' I.@:= ])})
bin =. BINPATH_z_ =. '/.guix-profile/bin',~home=. 2!:5'HOME'
install=. home,'/.guix-profile/share/j'
'addons system tools'=. install&, &.> '/addons';'/system';'/tools'
user=. home,userx=. '/j903-user'
'break config snap temp'=. user&, &.> '/break';'/config';'/snap';'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'
SystemFolders_j_=: ids,.jpathsep@".&.>ids
md=. 3 : 0
if. -.#1!:0 }:a=.y,'/' do. for_n. I. a='/' do. 1!:5 :: [ <n{.a end. end.
)
md &.> (user,'/projects');break;config;snap;temp NB. should handle in emacs?
0!:0 <jpathsep (4!:55 (;:'userx ids md'), ids)]system,'/util/boot.ijs'
require 'viewmat'
VISIBLE_jviewmat_ =: 0 NB. suppress viewmat from trying to open file itself

NB. letters
az =: a.{~97+i.26
AZ =: a.{~65+i.26
a09 =: a.{~48+i.10
link =: '[[file:'&, @ ,&']]'
linki =: 4 : 0
  NB. x linki y: write image y to file x. intended for org mode
  require 'ide/qt/qtlib'
  y writeimg_jqtide_ x
  link x
)
