# Formatting Files for Input

At present, `kwak-gui-file` only works on files that are formatted
as plain text. For the most part, this means `.txt` files (e.g.
those created in notepad), as opposed to `.doc`, `.rtf`, `.docx`, etc...

It also doesn't work on files that contain a mixture of English and
Kwak'wala, as the program will try to parse the English text as
though it were Kwak'wala, and change it around. You can prevent this
by putting pipes (the character `|`) around English text, but that
can be time-consuming depending on the amount of English names/etc...
You also can't convert files that contain multiple different 
orthographies in them, as there's no way to tell the program where
the different orthographies are in the file.

In general, the best way to work with a mixture of English and 
Kwakwala is to use a spreadsheet program, such as Microsoft 
Excel or LibreOffice Calc. Generally, you'll have a file with
one column for English, and one or more columns for Kwak'wala.
You can strip out the English text (and, if applicable, Kwak'wala
text in other orthographies), save as a `.csv` file, and then
use `kwak-gui-file` to convert to a different orthography. You
can the re-open the new converted `.csv` file in your spreadsheet
program, and copy the converted cells to where you want them.

For step-by-step instructions on how to convert your spreadsheets
to `.csv` and run them through the program, there is [a tutorial](csv/win-libre.md)
on how to do so using LibreOffice Calc on Windows. There should be
tutorials for other platforms and programs soon.

## Tutorial Links

### Converting Spreadsheets

  - Windows
    - [LibreOffice Calc](csv/win-libre.md)
    - Microsoft Excel
  - Mac OS
    - Microsoft Excel
    - LibreOffice Calc

