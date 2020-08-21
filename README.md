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


**Links:**
- Official Multiwfn website: http://sobereva.com/multiwfn
- Official Multiwfn English Forum: http://sobereva.com/wfnbbs/
- Official Multiwfn Chinese Forum 中文论坛: http://bbs.keinsci.com/wfn

# LICENSE
**Make sure to check the latest license from official Multiwfn website before you publish!**

LICENSE INFORMATION: To use Multiwfn, you are required to read and agree the following terms:
(a) Currently Multiwfn is free of charge and open-source for both academic and commerical usages, anyone is allowed to freely distribute the original or their modified Multiwfn codes to others.
(b) Multiwfn can be distributed as a free component of commercial code. Selling modified version of Multiwfn may also be granted, however, obtaining prior consent from the original author of Multiwfn (Tian Lu) is needed.
(c) If Multiwfn is utilized in your work, or your own code incorporated any part of Multiwfn code, at least the original paper of Multiwfn MUST BE cited in your work or code: Tian Lu, Feiwu Chen, J. Comput. Chem., 33, 580-592 (2012).
(d) There is no warranty of correctness of the results produced by Multiwfn, the author of Multiwfn does not hold responsibility in any way for any consequences arising from the use of the Multiwfn.


---------------------------------------------
Below are the license information in Chinese:

版权信息：在开始使用Multiwfn前，用户必须阅读并且接受以下条款：
(a)目前Multiwfn是对学术用户和商业用户都完全开源免费的程序，任何人都可以向他人免费传播原版或者其自己的修改版的Multiwfn程序。
(b)Multiwfn可以作为商业程序中的一个免费组件发布。售卖修改版Multiwfn也可以，但必须事先获得Multiwfn开发者（卢天）的同意
(c)如果Multiwfn在你的研究中被使用，或者你自己写的代码里利用了Multiwfn中的代码，至少要引用Multiwfn原文：Tian Lu, Feiwu Chen, J. Comput. Chem., 33, 580-592 (2012)
(d)Multiwfn开发者不保证Multiwfn计算结果的正确性，也不对因为使用Multiwfn给出的结果所导致的任何后果负责。（但开发者总会尽最大努力保证程序计算结果的正确性）

重要提示：如果使用了Multiwfn的文章中就连上面提到的Tian Lu, Feiwu Chen, J. Comput. Chem., 33, 580-592 (2012)这篇必须引的原文都没引用的话，作者会被列入Multiwfn黑名单，并禁止在未来使用Multiwfn。Multiwfn最正确的引用方式参见Multiwfn可执行文件包中的How to cite Multiwfn.pdf文档。

如果有Multiwfn使用上的问题，欢迎到http://bbs.keinsci.com的“波函数分析与Multiwfn”版块发帖咨询。开发者不在其它任何中文论坛上解答Multiwfn的使用问题。初次接触Multiwfn者请务必阅读《Multiwfn入门tips》（http://sobereva.com/167）和《Multiwfn FAQ》（http://sobereva.com/452）。

## Structure of the repository

Linux binary: /<br/>
Examples: /<br/>
Manual: /<br/>
Linux source code: /src<br/>

 

