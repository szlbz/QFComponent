 **QFComponent for lazarus**   
秋风(QQ:315795176)开发的控件包，有2个控件：  
这个控件包采用自定义的类富文本，集编辑、显示和导出图片等功能。  
TQFRichView：类RichView控件  
TQFScrollingText：滚动显示控件  

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
初步 **支持markdown格式的表格**   
图像格式支持： **jpg,bmp,png** 等    
可在windows和linux使用，已在龙芯电脑实测可用。  
TQFRichView控件支持鼠标中键滚动及按鼠标左键然后上下移动鼠标实现快速滚动。  
2024.03.09  
注意：  
1、文字显示渲染等核心功能是秋风独立编写的。  
2、QFRichEditor参考了https://github.com/DreamVB编写的Markdown Editor部分界面及代码  
3、滚动控件的滚动部分参考了lazarus about的算法。  
![截图](%E6%88%AA%E5%9B%BE.png)  
![截图2](%E6%88%AA%E5%9B%BE2.png)  
![截图3](%E6%88%AA%E5%9B%BE3.png)  
![截图4](%E6%88%AA%E5%9B%BE4.png)  
![截图4](%E6%88%AA%E5%9B%BE5.png)  
![截图4](%E6%88%AA%E5%9B%BE6.png)  
