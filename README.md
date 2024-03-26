QFComponent for lazarus    
最新的简介在：  
https://www.cnblogs.com/qiufeng2014/p/18058856     

秋风(QQ:315795176)开发的控件包,采用自定义的类富文本，集编辑、显示和导出图片等功能。  
QFComponent 有2个控件：  
1、TQFRichView：自定义的类富文本文字显示，类RichView控件，这个控件特别适合作为使用说明等用途，比用html或pdf都灵活且特别是跨平台使用；  
2、TQFScrollingText：滚动内容采用自定义的类富文本文字显示的滚动显示控件；  
*****
下载：  
https://github.com/szlbz/QFComponent.git  
https://gitee.com/szlbz/qfcomponent/  
*****
说明：  
1、控件的文字显示渲染等核心功能是秋风独立原创编写的。  
2、QFRichEditor参考了https://github.com/DreamVB编写的Markdown Editor界面及代码  
3、滚动控件的滚动部分参考了lazarus about的算法。  
*****
控件功能慢慢添加中!  
*****
更新说明：  
2024.03.25  
1、增加简单的打印功能  
2、完善书签跳转及超链接的功能  
3、增加超链接符号  
<hlk>xxxx</hlk>  
完善后的超链接可以在文本行(包括在表格的单元格)中任意位置出现  
2024.03.23  
添加书签跳转功能  
<BMx>--定义书签名称  
[BMx]--定义书签  
例子：  
<BM1>书签目录1  
<BM2>书签目录2  
[BM1]测试书签跳转功能  
[BM2]书签跳转功能正常  
2024.03.22  
增加多URL超链接识别，现在支持多个url点击。  
2024.03.21  
1、增加行距  
2、增加背景图片功能  
2024.03.20  
修正打开链接的Bug及修正滚动到最后的衔接Bug.  
2024.03.19  
增加表格单元格插入图片功能  
*****
使用以下特定符号定义文字（行）的属性：  
[img]本地图像文件名称  
[!]下划线  
[@]删除线  
[#]粗体  
[$]斜体  
[L]行居左  
[C]行居中  
[R]行居右  
[C1]黑色  
[C2]红色  
[C3]黄色  
[C4]绿色  
[C5]蓝色  
[S1]字体尺寸9  
[S2]字体尺寸12  
[S3]字体尺寸14  
[S4]字体尺寸16  
[S5]字体尺寸18  
[LINE]分割线  
[2LINE]双线条分割线  
定义文字颜色、样式：  
<C1>xxx</C>  
<C1>黑色  
<C2>红色  
<C3>黄色  
<C4>绿色  
<C5>蓝色 
</C>恢复原来的文字颜色  
<!>下划线  
<@>删除线  
<#>加粗  
</>恢复原来的文字样式  
支持markdown格式的表格   
图像格式支持：jpg,bmp,png等    
可在windows和linux使用，已在龙芯电脑实测可用。  
TQFRichView控件支持鼠标中键滚动及按鼠标左键然后上下移动鼠标实现快速滚动。  
2024.03.09  
*****
![截图1](%E6%88%AA%E5%9B%BE.png)  
![截图2](%E6%88%AA%E5%9B%BE2.png)  
![截图3](%E6%88%AA%E5%9B%BE3.png)  
![截图4](%E6%88%AA%E5%9B%BE4.png)  
![截图4](%E6%88%AA%E5%9B%BE5.png)  
![截图4](%E6%88%AA%E5%9B%BE6.png)  
