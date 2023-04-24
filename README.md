# Kwak-Orth GUI Applications

This repository contains two GUI applications to make using [kwak-orth](https://github.com/Anteproperispomenon/kwak-orth) 
easier. 

If you have a pre-compiled version of this program, you can skip ahead to the 'Usage' section
for information on how to use the programs.

## Compiling

Unfortunately, compiling can be a pain. First, you'll have to install Haskell and stack, for which there are instructions
on the [Haskell website](https://www.haskell.org/downloads/). 

Note that if you are trying to compile on Apple Silicon (i.e. a newer Mac with `M1` or `M2` etc...), 
there are more specific instructions [here](docs/apple-silicon.md).

Once you have Haskell and Stack installed, next you'll have to install the dependencies for 
[monomer](https://github.com/fjvallarino/monomer), the widget toolkit used by this program. Insructions
on how to install them can be found at <https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md>.
If you are having trouble installing monomer on Windows, you can try the steps I took at <https://github.com/fjvallarino/monomer/issues/201>.

Once you have set up all the dependencies, you should be able to compile `kwak-gui` by running
`stack build`. You can also compile and run one of the applicatons by running `stack run kwak-gui-text`
or `stack run kwak-gui-file`.


## Running

As stated before, you can run the applications by running `stack run ...`. However, you'll likely
want to be able to run the program without having to run stack each time. To do so, you'll have
to copy the executables to another location, along with some other files.

### Windows

On Windows, the executables can be found in `kwak-gui/.stack-work/install/.../bin/`. The `...`
represents a string of characters that change depending on aspects of how the code was built.
To figure out which directory to select (if there are more than one), check for the line

``` installing executable kwak-gui-text in ...\kwak-gui\.stack-work\install\...\bin ```

when compiling the application, or check in each to see which directory has the most
recently modified executables. Confusingly, the `date modified` field for the
directory may be older than the `date modified` field for the executables. Generally,
the string should stay the same between compilations unless you significantly change the
dependencies of the project. Thus, if you are just compiling the program and not modifying 
the code, there will probably only be one such directory.

After you find the executables, copy them to another directory  where you want to run
the program (hereafter referred to as the 'running directory'). Next, go back to the 
root of repository, and copy the `assets` folder to the running directory. Finally, 
you'll need two `dll` files for the programs to run. First, you'll need `sdl2.dll`,
which can be found at <https://github.com/libsdl-org/SDL/releases>. Download the latest
`SDL2-<version>-win32-x64.zip`, extract it, and copy `sdl2.dll` to the running
directory. Next, you'll need `glew32.dll`, which can be found at 
<https://github.com/nigels-com/glew/releases>. Again, download the latest
`glew-<version>-win32.zip` and extract it to a different folder. In the extracted
folder, go to `glew-<version>\bin\Release\x64` and copy `glew32.dll` to the
running directory. This should be enough for the executables to run; try running
them and check that they look the same as when running them with `stack run ...`.

You can then run the applications from the running directory from now on, or
compress them into a zip file to distribute to other users.

### Mac OS

Fortunately, running the applications via `stack run ...` works the same as it
does on Windows. Unfortunately, running the program via an executable is more
difficult than it is on Windows. Note that in order to see `.stack-work`, you'll
have to ensure that hidden files are visible. You can do this by pressing
`Command+Shift+Dot` while in Finder.

First, follow the same directions for copying executables as on Windows, aside
from the dll files. As before, the directory you are copying the files to will
be referred to as the 'running directory'.

Next, you'll need to copy files from `mac-run` in the repository to the
running directory. This is because executables on Mac don't run in the
directory they are located in, and thus can't see the 'assets' folder.
The `.command` files from the `mac-run` folder change the current directory
to the directory they are run in, and then run the program. Note that this
means you can't run the executables themselves directly, so you may wish
to hide them if you're planning on distributing the program to other people.

#### Visible Executables

This works if you're just running them for yourself and understand that you 
can't run the executables directly. Copy `kwak-gui/mac-run/kwak-gui-file.command`
and `kwak-gui/mac-run/kwak-gui-text.command` to the running directory, and then
run the `.command` files to use the programs. You won't need a `.dll` file or
any equivalent, since installing the monomer dependencies with homebrew make
the required code available to any programs that use them.

#### Hidden Executables

This (likely) works better if you plan to compress the program to a zip file
and distribute it to others. However, you will have to rename the executable
files each time you copy them over, so maybe don't use this method if you're
planning on re-compiling the programs frequently.

First, rename `kwak-gui-file` and `kwak-gui-text` to `.kwak-gui-file` and
`.kwak-gui-text` respectively (i.e. add a `.` to the beginning of the file
names). Next, copy `kwak-gui/mac-run/hidden/kwak-gui-file.command` and
`kwak-gui/mac-run/hidden/kwak-gui-text.command` from the repository to 
the running directory. Check that running the `.command` files runs the
programs as expected. 

If you wish to compress the files to a `.zip` archive and distribute it to
others, you'll likely want to include the files from `kwak-gui/mac-run-install`
in the archive. These files install `homebrew` and the monomer dependencies 
semi-automatically when run (the one that installs `homebrew` requires input
from the user). 

### Linux / Unix / BSD

I haven't yet tried compiling `kwak-gui` on linux or other unix-like systems
yet, but judging from WSL/Ubuntu, programs run in the directory they were
started in, so you shouldn't have to create shell scripts to run the programs
like you would on Mac OS.

## Usage

If you're compiling the repository, read the 'Running' section to figure out
how to run the program easily. If you have a pre-compiled version of the
program, you can ignore the 'Running' section. 

There are two executable files: `kwak-gui-file(.exe)` and `kwak-gui-text(.exe)`
(on Mac there are `.command` files). Use `kwak-gui-file` when you want to convert
plain-text files between orthographies. Use `kwak-gui-text` if you want to type in
your text directly or copy-and-paste text in and out.

### Common Features

Near the top, there are two lines of buttons. The `input` line is what orthography
your input text/file is in. The `output` line is what orthography you want the output
to be. You can mouse over each orthography button to get a short description of
the orthography in question.

If you wish to normalise/clean up some text, just select the same orthography for
both the input and output.

Also, there is a `config` button at the top/top-right. This has options that slightly
alter the output of the various orthographies. These options persist between uses
of the program, and between the two programs.

### Text-Based

In this version, there are two text boxes for the input and output. The box on the
left is where you type/paste text that you want converted.

If you want your text to be automatically converted, you can check the `Auto-Convert`
box at the top-right for text to be converted as you type/paste. If you don't turn
auto-convert on, you'll have to click `Convert` at the bottom to convert the text.
You can then click on `Copy` to copy the output text to your clipboard.

There is also a `Swap` button you can click that will swap the input/output orthography
and text. 

### File-Based

This version is intended for converting plain-text files directly to other orthographies.
As in the text-based version, this version contains two text boxes that represent the
input and output, but they are for display only, and cannot be edited. It also contains
two small text boxes that show the locations of the files you are working with.

#### Selecting File to Convert

To select the file you wish to convert, click `Select File` on the left side of the window.
On versions after 2023-04-24-1145, you can also press `Ctrl+O` (or `Cmd+O` on Mac).
This will open a system window where you can browse your computer for the file you are
looking for. Since this uses the system's `Open File` window, the interface will vary
depending on your operating system.

Note that this only works with plain-text files (i.e. `.txt` files in most cases); it
won't work with Microsoft Word files (i.e. `.doc` or `.docx` files) or any other 
formatted text files (e.g. `.rtf`, `.odt`, `.html`, etc...). It also requires that the
text files be in UTF-8 format. This shouldn't be a problem, since most modern operating 
systems use UTF-8 as the default encoding scheme. If you get an error opening a `.txt` file
, try opening the text file in Notepad (on Windows), and clicking `File > Save as`. Then, 
in the `Encoding` drop down, select `UTF-8` (**not** `UTF-8 with BOM`), and then save the
file to a different name. If it still doesn't work, you'll want to use the Text-Based
version instead, and just copy the text in, convert it, and copy it to the file you
want to output it to.

#### Selecting Output File

To save the output to a file, first you must click `Choose Destination` to select where
you want to save the file. Like with open, on newer versions you can press `Ctrl+S`/`Cmd+S`
to select this option. This opens your system's `Save File` window, which will ask
you to select a name and location for the output file. Note that this does **not**
actually save the file, even though it looks like it does. To save the file, you must
then click on `Save File` at the bottom of the main window. You can also press
`Ctrl+W`/`Cmd+W` to actually write the file to disk. Note that this awkward process
was originally set to change in a future version, but since there are now multiple
ways to select the output file, that seems unlikely.

Also note that, in versions since 2023-04-23, you can't select the input file as
output. This is to prevent accidental overwriting of the original file. It also
prevents data loss in case there is an error while overwriting the original file.

