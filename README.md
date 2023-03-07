# Kwak-Orth GUI Applications

This repository contains two GUI applications to make using [kwak-orth](https://github.com/Anteproperispomenon/kwak-orth) 
easier. 

## Compiling

Unfortunately, compiling can be a pain. First, you'll have to install Haskell and stack, for which there are instructions
on the [Haskell website](https://www.haskell.org/downloads/). 

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
have to ensure that hidden files are visible.

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
like you would on MacOS.

