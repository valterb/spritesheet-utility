# spritesheet-utility
Create spritesheet and view its animations. Flip and resize images. Built with "Lazarus" ( https://www.lazarus-ide.org/ ) and "Castle Game Engine" ( https://castle-engine.io/ ).
Tested on Windows 10 64bit, Lazarus Ver. 1.8.4, FPC Ver. 3.0.4.

With this application you can create spritesheets for <strong>Castle Game Engine</strong> (or other programs that use this format) directly from Lazarus without using the command line.<br />
Castle Game Engine is required to compile because some of its features are used.<br />
To create spritesheets combine_image_into_sprite_sheet (attached to this repository) is used. You can find the code in the CGE examples folder (as command line).<br />
<br />
Program has two sections, <em>Create spritesheet</em> and <em>Preview spritesheet</em>.<br />
<br />
The first one includes a small utility that allows you to flip and resize individual images and then save them.<br />
To create a spritesheet prepare a folder with the images required (can be anywhere on the disk, does not need to be in the application folder).<br />
As described in the CGE Guide ( https://castle-engine.io/manual_intro.php ), image names must end with an underscore followed by a numerical index (e.g. image_1.png, image_2.png). It is good practice to use zeros to complete the number if the images are more than 9 (... image_09, image_10 ...).<br />
Open any image in the series then define Padding and Columns.
<ul>
<li>Padding, will be the number of numbers after the underscore (image_09.png will have Padding = 2).</li>
<li>Columns, will be the number of columns you want to get in the spritesheet (number of frames in a row in the image).</li>
</ul>  
Give a name to the spritesheet (a default name is proposed) then click on Execute.<br />
If all settings are correct a folder will be created in the source folder with the generated spritesheet.<br />
As mentioned you can edit each image individually (select the desired image in the ShellView) and then save it (useful if you have drawn sprites in one direction and you want to create a spritesheet with the turned sprites). Also in this case a folder will be created automatically in the source folder.<br />
The resize function allows you to change the interpolation mode using values included in the Castle Game Engine (https://castle-engine.io/apidoc/html/CastleImages.html#TResizeInterpolation ). If you select Constrain the image will maintain its proportions.<br />
<br />
With <em>Preview spritesheet</em> you can view the sprite animation.<br />
Open the spritesheet (then select it in the ShellView).<br />
Set the required values:<br />
<br />
<ul>  
<li>Frame count, number of images</li>
<li>Columns, number of frames in a row in the image</li>
<li>Horz size frame, width size of the single frame</li>
<li>Vert size frame, height size of the single frame</li>
<li>Frame per sec, number of frames per second</li>
</ul>
By changing Frame count and Columns, values for Horz size frame and Vert size frame will be automatically proposed. You can still change them manually.<br />
Click Start to see the animation.<br />
You can change frames per sec even while the animation is active.<br />
<br />
