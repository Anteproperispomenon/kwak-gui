# Converting Tables with LibreOffice Calc

To convert excel/calc/spreadsheet files between 
orthographies, you'll need to strip out the English
cells and convert them to CSV format. This guide shows
you how to do this in LibreOffice Calc.

## Converting to CSV

First, you need to open the spreadsheet in LibreOffice Calc.

![File in LibreOffice Calc](../imgs/calc/calc_0001.png)

Then, select the first English cell. Next, hold `Shift`
and select the last English cell

![Selecting the English Cells](../imgs/calc/calc_0002.png)

Then, press `Delete` to strip the English cells away. 
If you have multiple discontinuous sections of English,
you'll have to repeat this for each section.

![Deleting the English Cells](../imgs/calc/calc_0003.png)

Next, you need to save this file as a `.csv` file. To do
this, first click `File` in the top left-hand corner
and select `Save as...`. Alternatively, you can press
`Ctrl+Shift+S`.

![Selecting `Save as...`](../imgs/calc/calc_0004.png)

This will bring up a dialog where you can select where
to save the file. First, navigate to the directory where
you wish to save the file, click the drop-down box
next to `Save as type`, and select `Text CSV (*.csv)`.

![Save File Dialog](../imgs/calc/calc_0005.png)

Next, name the file as you want, and click `Save`.

![Save File Dialog](../imgs/calc/calc_0006.png)

This (may) open a dialog asking if you want to save as a 
`.csv`. Just click `Use Text CSV Format` to continue.

![Confirm File Format](../imgs/calc/calc_0006b.png)

Next, the following dialog will come up. Make sure that
`Character set` is set to `Unicode (UTF-8)`. 
`Field Delimiter` should be set to `,`. These should
already be the settings selected, but in case they aren't,
set them as displayed. Then, click `OK` to save the file.

![CSV Settings](../imgs/calc/calc_0008.png)

## Converting Orthography

Next, you need to run `kwak-gui-file` to convert the
`.csv` file to a different orthography.

First, open up `kwak-gui-file.exe` in its directory.

![Opening `kwak-gui-file`](../imgs/gui/gui_0002.png)

![Running `kwak-gui-file`](../imgs/gui/gui_0003.png)

Next, select the input and output orthographies. If you
mouse overt the orthographies, a brief description will
pop up.

![Selecting Input Orthography](../imgs/gui/gui_0004.png)

![Selecting Output Orthography](../imgs/gui/gui_0005.png)

Then, click on `Select File` to open up a dialog to
select the file you want.

![Selecting Input File](../imgs/gui/gui_0006.png)

This will open up the file in, allowing you to see its
contents, and a preview of the converted content. If
either the input or output orthography is incorrect,
you can select the correct orthographies.

![Conversion Preview](../imgs/gui/gui_0007.png)

Now, you can select `Choose Destination` to choose
where to save, and its file name.

![Choose Destination](../imgs/gui/gui_0008.png)

You can type the new file name in the `File Name`
box. Remember to end the filename in `.csv` to
ensure that it is interpreted as such when
re-importing it.

![Choose File Name](../imgs/gui/gui_0009.png)

This will add the destination file into the small
box directly below the `Choose Destination` button.

Now, you just need to press `Save File` at the bottom
of the window, which will actually output the converted
file.

![Save File](../imgs/gui/gui_0010.png)

If the file saved successfully, a box that says
`File Saved Successfully` should pop up. 

![File Saved](../imgs/gui/gui_0011.png)

If the file doesn't save correctly, an error message 
may show up. If this happens, you can close the program
and try again.

## Re-opening in LibreOffice

Now, go into LibreOffice Calc and click `File > Open`
or press `Ctrl+O`. This will allow you to open the
`.csv` file that you just saved from `kwak-gui-file`.

![CSV Settings](../imgs/calc/open_calc_0001.png)

This will open up a window where you select the options
for importing the `.csv` file's contents. You can
probably leave this as-is, but you may want to de-select
`Semicolon` if your text contains semicolons. After 
selecting the options correctly, click `OK` to import
the file.

![CSV Settings](../imgs/calc/open_calc_0002.png)

Now, you have your converted cells in the new orthography.
You can again copy the cells to another file, or do
whatever you want with the data in question.

![CSV Settings](../imgs/calc/open_calc_0003.png)

