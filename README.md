
## Moon scripting language compiler and VM

[![Build Status](https://travis-ci.org/glampert/moon-lang.svg)](https://travis-ci.org/glampert/moon-lang)

`Moon` is a custom scripting language that borrows some of its syntax from
[Lua](http://www.lua.org/) and [Rust](https://www.rust-lang.org/).

This is mainly a toy/hobby project to learn more about compiler design, Virtual Machines and interpreters.

The name is just a play on the meaning of Lua (Moon in Portuguese). I chose the name on purpose
to leave it clear this is a lame rip-off on the syntax of the popular scripting language.
This project is also in no way affiliated to [MoonScript](https://github.com/leafo/moonscript).

### Building

To build the project on Mac and Linux, use the provided Makefile. Before building, make sure you have:

- [Flex](http://flex.sourceforge.net/) version `2.6` or newer installed.
- [Bison](http://www.gnu.org/software/bison/) version `3.0.4` or newer installed.
- A C++11 compiler. GCC v5 or above is recommended for Linux and Clang v6 or newer for Mac OSX.

To install Bison and Flex on Mac OSX, [brew](http://brew.sh/) is the tool of choice. If you experience problems
with the system path, make sure to check out [this thread](http://stackoverflow.com/a/29053701/1198654).

On Linux, you can use `apt` or whichever is your package manager of choice.
You might need to download and build Bison yourself if a recent binary image is not available.

**Building on Windows** is currently not officially supported. The source code should be
fully portable, so all that should be required for a Windows build is to install Bison
and Flex and create a Visual Studio project. You should be able configure Bison and Flex
to run as prebuild steps.

### License

This project's source code is released under the [MIT License](http://opensource.org/licenses/MIT).

