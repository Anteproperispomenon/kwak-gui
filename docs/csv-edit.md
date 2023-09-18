# Using `kwak-gui-csv`

`kwak-gui-csv` is the main way to edit spreadsheets and perform automatic orthography conversions. You can copy columns, convert
columns to different orthographies, delete unnecessary columns,
move columns around, or just leave columns alone.

## Before Using `kwak-gui-csv`

First, you'll need to convert your spreadsheet to `.csv` format.
This is fairly easy in a program like LibreOffice Calc or
Microsoft Excel. When converting, these programs will usually
give you options on how to export the data. The most common option
is what to use as the cell seperator. The most common option is to
use the comma character ( `,` ), followed by the semi-colon ( `;` ).
Using comma is probably the best choice, but if you have problems
when using it, you can try exporting with a different separator.
Remember what character you use as your separator, as you'll need
to enter it when using `kwak-gui-csv`.

If your spreadsheet is already in `.csv` or `.tsv` format, you can
(probably) import the file directly into `kwak-gui-csv`.

(Maybe add pictures of the process here)

## Starting Up `kwak-gui-csv`

When starting up `kwak-gui-csv`, you'll (hopefully) see something like
the following screen: 

![Start Screen](imgs/csv/kwak-gui-csv-001-start.png)

To start, you'll need to click `Select File`, which will open up
your OS's `Open File` window. On Windows, this will look something
like this: 

![Open File (Windows)](imgs/csv/kwak-gui-csv-002-open1.png)

Make sure you select the file you want to use / just converted
to `.csv`, then click the `Open` button. This will bring up the
import options dialog, seen below.

![Import Options](imgs/csv/kwak-gui-csv-004-import-1.png)

Be sure to use the same separator character as you used when
converting the file. If you don't remember or didn't save the
file yourself, you'll have to guess at what the separator is.
If you get it wrong, the import result will look something like
this:

![Incorrect Import](imgs/csv/kwak-gui-csv-005-import-error.png)

If this happens, you'll have to click `Select File` again, choose
the same file, and choose a different separator.

![Trying a different separator](imgs/csv/kwak-gui-csv-006-import-2.png)

The other option in the import dialog is the `Get Headers from File`
option. This (admittedly poorly named) option changes how the first
row of the spreadsheet is imported into the file. If it isn't selected,
the first row will be treated like any other row in all columns. This
is useful when your spreadsheet doesn't have a header for each row,
or the header is in the same orthography as the rest of the column.
On the other hand, if all your headers in the first row are all in
English, you'll want to turn/leave this option on.

![Importing](imgs/csv/kwak-gui-csv-007-import-3.png)

Note that if you turn off `Get Headers from File`, the program will
still show headers, but they will be generic headers, with the format
`Column N`, where N is the internal number of the column. 

## Editing Columns

After importing the file, the program will then look something like
this:

![After Importing](imgs/csv/kwak-gui-csv-008-edit-start.png)

The window will be divided into columns, with one column for each
column in the `.csv` file. Each column contains, from top to bottom,

  - The header name / column number
  - The `Swap`/`Move` handles
  - The `Modify` switch
  - The Input Orthography Selector
  - The Output Orthography Selector
  - The `Copy`/`Delete` buttons
  - The `.csv` data for that column

### Deleting Excess Columns

Sometimes, you'll have an empty column, or a column that is a duplicate
of another column. To delete these excess columns, just scroll to the
appropriate column, and click `Delete`.

![Clicking Delete](imgs/csv/kwak-gui-csv-009-delete.png)

This will bring up a confirmation dialog, ensuring that you actually
want to delete this column. To go ahead with deleteing, just click 
`Delete`...

![Configrming Delete](imgs/csv/kwak-gui-csv-010-delete-2.png)

... and the column will be removed. Note that, when using generic
headers, this may cause your columns to not have sequential column
numbers. This isn't a problem; it's just a result of how the columns
are stored in the program's memory. When saving the file, the deleted
column will not appear in the output.

![Post-Delete / Non-Sequential Numbers](imgs/csv/kwak-gui-csv-011-delete-3.png)

### Moving Columns Around

Sometimes, the columns won't be in the order you want. This can be changed
by using the `Move` and `Swap` handles. Unlike most of the other
interactive elements, these are not buttons, but rather 'handles' that
can be dragged and dropped to other columns.

When dragging `Swap` to another column (hereafter known as the target 
column), the two columns positions will simply be switched with each 
other. 

![Dragging Swap 1](imgs/csv/kwak-gui-csv-012-swap-1.png)

![Dragging Swap 2](imgs/csv/kwak-gui-csv-013-swap-2.png)

![Dragging Swap 3](imgs/csv/kwak-gui-csv-014-swap-3.png)

`Move`, on the other hand, leaves the target column in the same spot,
but moves the initial column to the space immediately before the target
column. This is useful if you want to change the position of an
orthography in a longer list. Note that if you want to move a column
to the end, you can drag the `Move` handle to the last column, and
then `Swap` the last two columns.

For more information on `Move`, see the appendix.

### Copying a Row as a Different Orthography

Generally, when using this program, you'll want to add new columns that
are just existing columns, but converted to a different orthography.

There are two ways to do this.
  - Click copy and then change the output orthography of the new column.
  - Change the output orthography of the initial column, copy it, and then change the initial column back to its original orthography.

While the first option seems easier, the second option has benefits 
as a result of how the underlying code works. Basically, each column
has "input" text and "output" text. If the `Modify?` button
is on, then the output text is displayed in the text box in the column; 
otherwise, the input text is displayed. When following the second option,
the output text of the initial column is copied to the __input__ text of
the new column. This means that the new orthography will be displayed
in the new column, even if the `Modify?` option is deselected.

If you didn't understand that, that's okay. Just follow the guide below
for copying a column as a new orthography.

#### Copying Guide

First, turn on the `Modify?` button for the column you want to copy.
Under `Input`, select the correct orthography of the column.

![Turning on `Modify`](imgs/csv/kwak-gui-csv-016-mod-2.png)

(Note: the third row was deleted between these two images)

![Where to select the input orthography](imgs/csv/kwak-gui-csv-019-mod-3.png)

Next, select the `Output` dropdown and select the target orthography
of the new column.

![Output Orthography Selected](imgs/csv/kwak-gui-csv-020-copy-1.png)

After that, click the `Copy` button to start the copy process. This
will bring up a dialog giving you a few options.

![Copy Dialog Options](imgs/csv/kwak-gui-csv-021-copy-2.png)

Copying from Output is the default option, and usually what you
want. Note that if you don't have `Modify?` selected, the program
will copy from Input regardless of your selection here.

You can also give the Column a header. This is more useful
when you selected to take your headers from the file, but
can make it easier to identify your columns in the program.
In newer versions, you can rename the header of any column,
allowing you to create headers yourself for all columns.
Leaving the `Header` field blanks just assigns a generic
header to the column.

![Entering a new Header Name](imgs/csv/kwak-gui-csv-022-copy-3.png)

After optionally entering in the header name, you can then click
`Copy Column` to actually copy the column. 

![Copied the Column](imgs/csv/kwak-gui-csv-023-copy-4.png)

You may notice that this new columns Input orthography is the same
as the output orthography of the initial column. This is correct
(assuming you selected `Modify?` and copy from output for the initial
column). Now, all that is left to do is to change the initial column
back to its original orthography. You can either just deselect `Modify?`,
change its Output orthography to be the same as its Input orthography,
or both.

![After copying another column](imgs/csv/kwak-gui-csv-027-copy-8.png)

![Reverting the Output Orthography](imgs/csv/kwak-gui-csv-028-mod-4.png)

![Deselecting `Modify`](imgs/csv/kwak-gui-csv-029-mod-5.png)

![Done!](imgs/csv/kwak-gui-csv-030-mod-6.png)

## Saving

After finishing up your spreadsheet, you'll have to save it if you
actually want to keep the changes around. To do so, just click the
big `Save File` button at the bottom of the window.

![Clicking Save](imgs/csv/kwak-gui-csv-032-save-1.png)

This will bring up your Operating System's `Save File` dialog, which
should be familiar to you. On Windows, to save, just enter in a new
file name and click `Save`.

![Saving a file on Windows](imgs/csv/kwak-gui-csv-033-save-2.png)

After clicking `Save`, the program will bring up an "Output Settings"
dialog similar to the "Import Settings" dialog when you opened the file.
Usually, the same options will be selected as when you impoted the file.
You can change them if you want to change the separator character used,
or to save the new headers you created while editing the columns.

![Output settings](imgs/csv/kwak-gui-csv-034-save-3.png)

![Changing the output settings](imgs/csv/kwak-gui-csv-035-save-4.png)

Once you're done changing the settings around, you can just click
`Save File` on the dialog to actually save the file to your computer.
After clicking save, an alert should pop up saying that the file was
saved successfully

![Successful Save](imgs/csv/kwak-gui-csv-036-save-5.png)

After seeing this message, you can then open the `.csv` file back
up in your preferred spreadsheet editor, and convert it to whatever
format you want.

## Appendix

### More details on `Move`

While using `Move` was documented in the main help in the section
on moving columns, there were no images for performing `Move`.
Instead, here's a demonstration of using `Move` to move columns around.

First, a spreadsheet with various columns is loaded up.

After that, we can click and drag the `Move` handle of Column 2...

![Successful Save](imgs/csv/kwak-gui-csv-037-move-1.png)

... and drag it on top of Column 4.

![Successful Save](imgs/csv/kwak-gui-csv-038-move-2.png)

This results in moving the second column to the left of the target 
column (the fourth column). Since the same thing could be 
accomplished by just swapping the second and third columns, 
we'll next illustrate a larger move.

![Successful Save](imgs/csv/kwak-gui-csv-039-move-3.png)

First, we click and drag on the `Move` handle on the second column 
(displayed as `Column 3` at the moment, due to the way the 
program handles moves)...

![Successful Save](imgs/csv/kwak-gui-csv-040-move-4.png)

... and then we drag it to the final column, making it the 
penultimate column.

![Successful Save](imgs/csv/kwak-gui-csv-041-move-5.png)

And we can just leave it there if that's where we want it.
However, if we want to make it the final column...

![Successful Save](imgs/csv/kwak-gui-csv-042-move-6.png)

... we can simply click and drag its `Swap` handle...

![Successful Save](imgs/csv/kwak-gui-csv-043-move-7.png)

... and drag it to the final column again.

![Successful Save](imgs/csv/kwak-gui-csv-044-move-8.png)

Now this column is the final column, as desired.

![Successful Save](imgs/csv/kwak-gui-csv-045-move-9.png)

