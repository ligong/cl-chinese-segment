A Chinese segement package in common lisp

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

Acknowledgements
=============
I learn the segment algorithm from Peter Norvig's excellent text 
http://norvig.com/ngrams/ch14.pdf

The chinese language model is taken from SunPingYing project

   
     





