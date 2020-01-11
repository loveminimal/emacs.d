;;; `cnfonts--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cnfonts-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
(setq cnfonts--custom-set-fontnames
      '(
        ("Consolas" "Monaco" "DejaVu Sans Mono" "Droid Sans Mono" "PragmataPro" "Courier" "Courier New" "Ubuntu Mono" "Liberation Mono" "MonacoB" "MonacoB2" "MonacoBSemi" "Droid Sans Mono Pro" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT" "Iosevka Term" "Inconsolata-dz" "American Typewriter" "Menlo" "Anka/Coder Condensed" "Fantasque Sans Mono" "M+ 1m" "CamingoCode" "Office Code Pro" "Roboto Mono" "Input Mono" "Courier Prime Code" "NanumGothicCoding" "Monoid" "Edlo" "Iosevka" "Mononoki" "Robot Mono" "Fantasque" "Fira Code" "Go Mono" "Noto Sans Mono CJK" "InputMonoCompressed" "Hasklig" "Terminus" "FantasqueSansMono" "AnonymousPro" "3270" "Arimo" "D2Coding" "Inconsolata-g" "ProFont for Powerline" "Meslo" "Meslo Dotted" "Noto Mono" "Symbol Neu" "Tinos" "Space Mono" "SFMono Nerd Font")
        ("KaiTi" "微软雅黑" "Noto Sans S Chinese Regular" "Microsoft Yahei" "Microsoft_Yahei" "Ubuntu Mono" "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Source Han Sans SC" "Source Han Serif SC" "思源黑体 CN Regular" "思源黑体 CN Medium" "思源黑体 CN Normal" "思源宋体 CN" "思源宋体 CN Medium" "思源宋体 CN SemiBold" "Hiragino Sans GB" "文泉驿正黑" "文泉驿点阵正黑" "SimHei" "SimSun" "NSimSun" "FangSong" "FangSong_GB2312" "KaiTi_GB2312" "LiSu" "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti" "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti" "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文行楷" "华文彩云" "华文隶书" "华文细黑" "华文琥珀" "华文楷体" "华文新魏" "微软简标宋" "微软简粗黑" "微软简老宋" "微软简魏碑" "微软简楷体" "微软简行楷" "微软简隶书" "微软简中圆" "微软简仿宋" "方正大黑_GBK" "方正大标宋_GBK" "方正报宋_GBK" "方正彩云_GBK" "方正综艺_GBK" "方正稚艺_GBK" "方正幼线_GBK" "方正准圆_GBK" "方正中等线_GBK" "方正中倩_GBK" "方正行楷_GBK" "方正细珊瑚_GBK" "方正细圆_GBK" "方正细倩_GBK" "方正新舒体_GBK" "方正新报宋_GBK" "方正小标宋_GBK" "方正姚体_GBK" "方正魏碑_GBK" "方正舒体__GBK" "方正细黑一_GBK" "方正细等线_GBK" "方正水柱_GBK" "方正宋黑_GBK" "方正宋三_GBK" "方正宋一_GBK" "方正隶二_GBK" "方正隶书_GBK" "方正胖娃_GBK" "方正美黑_GBK" "方正瘦金书_GBK" "方正平和_GBK" "方正少儿_GBK" "方正书宋_GBK" "方正黑体_GBK" "方正黄草_GBK" "方正隶变_GBK" "方正琥珀_GBK" "方正楷体_GBK" "方正康体_GBK" "方正华隶_GBK" "方正仿宋_GBK" "方正超粗黑_GBK" "方正粗宋_GBK" "方正粗倩_GBK" "方正徐静蕾字体" "方正黑体简体" "方正黄草简体" "方正隶二简体" "方正隶书简体" "方正铁筋隶书简体" "方正超粗黑简体" "方正行楷简体" "方正艺黑简体" "方正胖娃简体" "方正胖头鱼简体" "方正美黑简体" "方正综艺简体" "方正细黑一简体" "方正细等线简体" "方正细珊瑚简体" "方正细圆简体" "方正细倩简体" "方正粗活意简体" "方正粗宋简体" "方正粗圆简体" "方正粗倩简体" "方正稚艺简体" "方正祥隶简体" "方正硬笔行书简体" "方正硬笔楷书简体" "方正瘦金书简体" "方正琥珀简体" "方正流行体简体" "方正水黑简体" "方正水柱简体" "方正北魏楷书简体" "方正华隶简体" "方正卡通简体" "方正古隶简体" "方正启体简体" "方正大标宋简体" "方正大黑简体" "方正姚体简体" "方正宋一简体" "方正宋黑简体" "方正小标宋简体" "方正少儿简体" "方正平和简体" "方正幼线简体" "方正康体简体" "方正彩云简体" "方正报宋简体" "方正新报宋简体" "方正新舒体简体" "方正楷体简体" "方正毡笔黑简体" "方正剪纸简体" "方正准圆简体" "方正仿宋简体" "方正书宋简体" "方正中等线简体" "方正中倩简体" "方正黄草简" "方正魏碑简体" "方正隶变简体" "汉仪彩云体简" "汉仪彩蝶体简" "汉仪报宋简" "汉仪柏青体简" "汉仪白棋体简" "汉仪长宋简" "汉仪长美黑简" "汉仪长艺体简" "汉仪粗仿宋简" "汉仪粗圆简" "汉仪粗宋简" "汉仪粗黑简" "汉仪超粗黑简" "汉仪超粗宋简" "汉仪超粗圆简" "汉仪陈频破体简" "汉仪嘟嘟体简" "汉仪大宋简" "汉仪大隶书简" "汉仪大黑简" "汉仪方叠体简" "汉仪方隶简" "汉仪蝶语体简" "汉仪黛玉体简" "汉仪仿宋简" "汉仪哈哈体简" "汉仪橄榄体简" "汉仪海韵体简" "汉仪琥珀体简" "汉仪花蝶体简" "汉仪黑咪体简" "汉仪黑棋体简" "汉仪凌波体简" "汉仪家书简" "汉仪楷体简" "汉仪漫步体简" "汉仪火柴体简" "汉仪立黑简" "汉仪菱心体简" "汉仪萝卜体简" "汉仪书宋一简" "汉仪书宋二简" "汉仪书魂体简" "汉仪南宫体简" "汉仪咪咪体简" "汉仪清韵体简" "汉仪瘦金书简" "汉仪神工体简" "汉仪双线体简" "汉仪太极体简" "汉仪娃娃篆简" "汉仪水波体简" "汉仪水滴体简" "汉仪特细等线简" "汉仪舒同体简" "汉仪魏碑简" "汉仪小隶书简" "汉仪秀英体简" "汉仪细中圆简" "汉仪细圆简" "汉仪细等线简" "汉仪细行楷简" "汉仪行楷简" "汉仪醒示体简" "汉仪丫丫体简" "汉仪中楷简" "汉仪中等线简" "汉仪中黑简" "汉仪圆叠体简" "汉仪雁翎体简" "汉仪雪君体简" "汉仪雪峰体简" "汉仪中圆简" "汉仪中宋简" "汉仪中隶书简" "汉仪竹节体简" "汉仪字典宋简" "经典行楷简" "经典魏碑简" "经典空叠圆简" "经典仿宋简" "经典细标宋简" "经典细等线简" "经典中宋简" "经典标宋简" "经典粗仿黑" "经典粗黑简" "经典超圆简" "经典长宋简" "经典叠圆体简" "经典等线简" "经典粗圆简" "经典粗宋简" "经典楷体简" "经典空叠黑" "经典黑体简" "经典平黑简" "经典空趣体简" "经典美黑简" "经典隶变简" "经典图案字" "经典宋体简" "经典特宋简" "经典特黑简" "经典舒同体简" "经典趣体简" "经典圆叠黑" "经典细空艺" "经典细空黑" "经典行书简" "经典细宋简" "经典细隶书简" "经典中圆简" "经典圆体简" "经典综艺体简" "经典细圆简" "长城中圆体" "长城黑宋体" "长城黑体" "长城细圆体" "长城长宋体" "长城行楷体" "长城行书体" "长城美黑体" "长城细仿宋体" "长城粗魏碑体" "长城粗隶书体" "长城粗行楷体" "长城粗圆体" "长城特粗黑" "长城特粗宋体" "长城特粗圆体" "长城特圆体" "长城楷体" "长城新魏碑体" "长城新艺体" "长城报宋体" "长城小标宋体" "长城小姚体" "长城宋体" "长城大黑体" "长城大标宋体" "长城仿宋体" "长城中隶体" "长城中行书体")
        ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")
        ))

;;; `cnfonts--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
(setq cnfonts--custom-set-fontsizes
      '(
        (9    10.5 10.5)
        (10   12.0 12.0)
        (11.5 13.5 13.5)
        (12.5 14.5 14.5)
        (14   16.5 16.5)
        (15   18.0 18.0)
        (16   19.5 19.5)
        (18   21.0 21.0)
        (20   24.0 24.0)
        (22   25.5 25.5)
        (24   28.5 28.5)
        (26   31.5 31.5)
        (28   33.0 33.0)
        (30   36.0 36.0)
        (32   39.0 39.0)
        ))
