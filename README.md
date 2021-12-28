# About this repository

This repository tracks the latest *Linux* version of Multiwfn and its manual (hereinafter referred to as *the Software*). I merely download *the Software* from official Multiwfn website and upload them to this repository without any change. I will try to update this repository as soon as possible when there is a new version published on Multiwfn website, however there is no guarantee.

**Disclaimer:**

The owner of this repository claims no right to *the Software*. All rights are reserved by the original developer Dr.Tian Lu. Also see below for license information. 

Should any information in this repository be in conflict with the official Multiwfn website, the official Multiwfn website should be taken as valid and binding.

**Install:**

A detailed guide is in Section 2.1.2 of the Multiwfn manual. Here's a short version for Ubuntu 18.04 LTS.

1. Clone this repository.
2. Install *motif* by running *sudo apt-get install libmotif-dev*. 
3. Add these lines to shell startup files: *export KMP_STACKSIZE=200M* and *ulimit -s unlimited*.
4. Add $Multiwfnpath environmental variable to shell startup files: *export Multiwfnpath=/path/to/multiwfn-folder*.
5. Add *Multiwfn* executable to $PATH. For example, *ln -s /path/to/Multiwfn-executable ~/.bin*.
6. Configure *setting.ini* according to the specific functions your need. Check the manual for details.


**How to Cite:**

Please carefully check "How to cite Multiwfn.pdf" document in the package. Do **NOT** cite this repository as this is not official. 


**Useful Links:**
- Official Multiwfn website: http://sobereva.com/multiwfn
- Official Multiwfn English Forum: http://sobereva.com/wfnbbs/
- Official Multiwfn Chinese Forum 中文论坛: http://bbs.keinsci.com/wfn


**Structure of the repository**

Linux binary: /<br/>
Examples: /<br/>
Manual: /<br/>
Linux source code: /src<br/>

 
# LICENSE
**Make sure to check the latest license from official Multiwfn website before you publish!**

LICENSE INFORMATION: To use Multiwfn, you are required to read and agree the following terms:
(a) Currently Multiwfn is free of charge and open-source for both academic and commerical usages, anyone is allowed to freely distribute the original or their modified Multiwfn codes to others.
(b) Multiwfn can be distributed as a free component of commercial code. Selling modified version of Multiwfn may also be granted, however, obtaining prior consent from the original author of Multiwfn (Tian Lu) is needed.
(c) If Multiwfn is utilized in your work, or your own code incorporated any part of Multiwfn code, at least the original paper of Multiwfn MUST BE cited in your work or code: Tian Lu, Feiwu Chen, J. Comput. Chem., 33, 580-592 (2012).
(d) There is no warranty of correctness of the results produced by Multiwfn, the author of Multiwfn does not hold responsibility in any way for any consequences arising from the use of the Multiwfn.

Whenever possible, please mention and cite Multiwfn in main text rather than in supplemental information, otherwise not only Multiwfn will be difficult for readers to notice, but also the paper will not be included in citation statistics.


Below are LICENSE information in Chinese:

使用了Multiwfn的文章里不引用或错误地引用Multiwfn程序的现象极为严重（尤其是在中国用户的文章中）！特此强调：

在研究中无论使用Multiwfn的哪个功能，在发表的文章的正文中都应当对Multiwfn进行正确引用，这是基本的学术道德规范。最全面、合理的引用Multiwfn程序及其作者的相关工作的说明见Multiwfn程序包中的How to cite Multiwfn.pdf文档。如果你发表的使用了Multiwfn的文章中连此文档中提到的Tian Lu, Feiwu Chen, J. Comput. Chem., 33, 580-592 (2012)这篇必须引的Multiwfn原文都没引用的话，将会被列入Multiwfn黑名单，并禁止在未来使用Multiwfn。

请在文章正文里提及和引用Multiwfn，而不要只放到补充材料里，否则不仅读者难以注意到，而且也不会被纳入引用的统计。

Multiwfn允许用于给别人代算时使用，但必须主动告诉对方在文章中需要明确引用Multiwfn程序。

开发Multiwfn花费了巨大精力和心血，恰当引用Multiwfn原文及作者的相关文章是对Multiwfn这个完全免费、不懈开发的程序的开发最好的支持！

---------
另：

如果有Multiwfn使用上的问题，欢迎到http://bbs.keinsci.com的“波函数分析与Multiwfn”版块发帖咨询，开发者会非常及时回复（通常在24小时内回复）。也欢迎在Multiwfn英文论坛http://sobereva.com/wfnbbs上用英语发帖求助、和外国Multiwfn用户交流。开发者不在其它任何其它中文论坛里解答Multiwfn的使用问题。

初次接触Multiwfn者请务必阅读《Multiwfn入门tips》（http://sobereva.com/167）。《Multiwfn FAQ》（http://sobereva.com/452）也非常重要，强烈建议完整过目一遍。

---------

Multiwfn的版权信息：
在开始使用Multiwfn前，用户必须阅读并且接受以下条款
(a)目前Multiwfn是对学术用户和商业用户都完全开源免费的程序，任何人都可以向他人免费传播原版或者其自己的修改版的Multiwfn程序
(b)Multiwfn可以作为商业程序中的一个免费组件发布。售卖修改版Multiwfn也不是不可以，但必须事先获得Multiwfn开发者（卢天）的同意
(c)如果Multiwfn在你的研究中被使用，或者你自己写的代码里利用了Multiwfn中的代码，至少要在发表的文章中引用Multiwfn的原文：Tian Lu, Feiwu Chen, J. Comput. Chem., 33, 580-592 (2012)
(d)Multiwfn开发者不保证Multiwfn计算结果的正确性，也不对因为使用Multiwfn给出的结果所导致的任何后果负责。（但开发者总会尽最大努力保证程序计算结果的正确性）
