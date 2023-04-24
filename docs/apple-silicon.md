# Compiling/Running on Apple Silicon

Compiling and running the code on Apple Silicon Macs
(newer Macs with `M1` or `M2` architecture) are currently
(March 2023) more difficult to compile the code on. There
are a number of reasons for this:

  - There is no direct compiler for Haskell to ARM, so
    it has to compile to [LLVM](https://en.wikipedia.org/wiki/LLVM) 
    first.
  - Currently, there is no official release of `Stack` for
    Apple Silicon. You can install an unofficial version
    via [ghcup](https://www.haskell.org/ghcup/).
  - Intel-based MacOS programs run on Apple Silicon via
    the Rosetta 2 emulator fine, so there isn't as much
    impetus to port tools to Silicon.
  - Installing certain tools automatically can install
    the Intel version of tools instead of the Silicon version.

In addition, the executable size of native Apple Silicon Haskell 
apps is significantly larger than Intel MacOS apps. As an example, 
the kwak-gui apps on Intel Mac are about 10Mb, while the Silicon
version apps are about 70Mb.

## Actually Compiling to Silicon

### Setting up Tools

First, you will have to install [Homebrew](https://brew.sh/).
This will allow you to install the various tools that 
you'll need to compile. 

```sh
brew install ghcup
brew install llvm@12
```

Afterwards, you'll have to add the location of `LLVM`
binaries to your `$PATH` variable. After installing 
`LLVM`, the terminal should give you a command to
do this. If it doesn't, the following command should
work:

```sh
export PATH=$PATH:"/usr/local/opt/llvm@12/bin"
```

if it doesn't, you can also try:


```sh
echo 'export PATH="/usr/local/opt/llvm@12/bin:$PATH"' >> ~/.zshrc
```

or

```sh
echo 'export PATH="/usr/local/opt/llvm@12/bin:$PATH"' >> ~/.bash_profile
```

depending on your shell.

### Installing Stack

You can find information on how to install `Stack` at
<https://docs.haskellstack.org/en/stable/install_and_upgrade/>,
and you can find the links for the unofficial builds of `Stack`
at <https://downloads.haskell.org/ghcup/unofficial-bindists/stack/>. As of writing, the latest 
version of `Stack` for Apple Silicon can be found at
<https://downloads.haskell.org/ghcup/unofficial-bindists/stack/2.9.3/stack-2.9.3-osx-aarch64.tar.gz>.

For example, to install the above version of Stack, you can
invoke `ghcup` like so:

```sh
ghcup install stack -u 'https://downloads.haskell.org/ghcup/unofficial-bindists/stack/2.9.3/stack-2.9.3-osx-aarch64.tar.gz'
```



