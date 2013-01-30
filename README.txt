A Chinese segement package in common lisp

Dependency
==============
Need open-gram project's language model
 1) Download lm_sc.t3g.arpa-xxxxxxxx.tar.bz2 from open-gram project home
https://code.google.com/p/open-gram/downloads/list
 2) mv the unziped file lm_sc.t3g.arpa to data directory of this project


API
==============
[Function]
segment text => word list
    segment chinese text into words
    example: 
    (segment "乒乓球拍卖完了") => ("乒乓球" "拍" "卖完" "了")

Special variable
=============
*language-model*
   defined in parameter.lisp, choose 'unigram or 'bigram language model
   bigram model takes more memory,but provide better results.
   default value is 'bigram.

*language-model-pathname*
  open-gram's language model pathname
   

Acknowledgements
=============
I learn the segment algorithm from Peter Norvig's excellent writing
http://norvig.com/ngrams/ch14.pdf

The chinese language model is taken from open-gram project
https://code.google.com/p/open-gram/downloads/list

   
     





