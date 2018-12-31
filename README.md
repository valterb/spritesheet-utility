# spritesheet-utility
Create spritesheet and view animations. Flip and resize images. Built with Lazarus and Castle Game Engine.
Tested on Windows 10 64bit, Seven 64bit, Vista 32bit.

With this application you can create spritesheets for Castle Game Engine (or other programs that use this format).
Castle Game Engine is required to compile because some of its features are used.
To create spritesheets combine_image_into_sprite_sheet (attached to this repository) is used. You can find the code in the CGE examples folder (as command line).

Program has two sections, Create spritesheet and Preview spritesheet.

The first one includes a small utility that allows you to flip and resize individual images and then save them.
To create a spritesheet prepare a folder with the images required (can be anywhere on the disk, does not need to be in the application folder).
As described in the CGE Guide, image names must end with an underscore followed by a numerical index (e.g. image_1.png, image_2.png). It is good practice to use zeros to complete the number if the images are more than 9 (... image_09, image_10 ...).
Open any image in the series then define Padding and Columns. 
Padding will be the number of numbers after the underscore (image_09.png will have Padding = 2).
Columns will be the number of columns you want to get in the spritesheet (number of frames in a row in the image).
Give a name to the spritesheet (a default name is proposed) then click on Execute.
If all settings are correct a folder will be created in the source folder with the generated spritesheet.
As mentioned you can edit each image individually (select the desired image in the ShellView) and then save it (useful if you have drawn sprites in one direction and you want to create a spritesheet with the turned sprites). Also in this case a folder will be created automatically in the source folder.
The resize function allows you to change the interpolation mode using values included in the Castle Game Engine. If you select Constrain the image will maintain its proportions.

With Preview spritesheet you can view the sprite animation.
Open the spritesheet (then select it in the ShellView).
Set the required values:
Frame count, number of images
Columns, number of frames in a row in the image
Horz size frame, width size of the single frame
Vert size frame, height size of the single frame
Frame per sec, number of frames per second
By changing Frame count and Columns, values for Horz size frame and Vert size frame will be automatically proposed. You can still change them manually.
Click on Start to see the animation.
You can change frames per sec even while the animation is active.

