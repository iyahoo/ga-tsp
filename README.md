ga-tsp
======

Genetic Programing - Traveling Salesman Problem

実行する場合、街の表示(draw-city関数)は Land of Lisp の graph-util を若干書き換えたものを使用しています。

下記リンクで公開されています。

http://landoflisp.com/graph-util.lisp

またcl-projectを利用しています。
qlのパスが通っているところに置き、

$ (ql:quickload :ga-tsp)

$ (in-package :ga-tsp)

$ (main)

で実行できます。

参考

Conrad Barski, M.D. 2013 Land of Lisp オライリー・ジャパン

Peter Seibel 2008 実践Common Lisp オーム社

荒屋真二 2004 人工知能概論(第２版) 共立出版

http://modern-cl.blogspot.jp/
