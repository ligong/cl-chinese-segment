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
    (segment "新疆维吾尔自治区乌鲁木齐国家高新技术产业开发区社会管理综合治理委员会学校及周边治安综合治理工作领导小组") => ("新疆" "维吾尔" "自治区" "乌鲁木齐" "国家" "高新技术" "产业" "开发区" "社会管理" "综合治理" "委员会" "学校" "及" "周边" "治安" "综合治理" "工作" "领导小组")

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

   
     





