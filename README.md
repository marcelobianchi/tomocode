tomocode
========

Telesseismic Tomography Code initially written by Andrey Jakovlev.

This code was adapted by Marcelo Bianchi to run on Linux environment and 
modified by Sofia Kufner.

The files on the SHELL folder are helpers to use the code on the Linux environment. Specially the tomo.bshm file. This file implements a bash shell function called tomo that allows you to more easily uses the code. To use it follow those steps:


1) First of all edit this file (line 27) and set the variable "tomobase" to the folder where you installed the code. In my case:

    ~ Begin tomo.bshm segment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ do not copy this line ~
    # Setup
    tomobase="/home/mbianchi/tomocode/"
    [ ! -d "${tomobase}" ] && echo "tomo.bshm: Invalid folder '${tomobase}', please edit tomo.bshm and set the tomoroot folder." && return 1
    export PATH=${tomobase}/SHELL:$PATH
    #
    ~ end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ do not copy this line ~

2) Install it. I have a library set of shell functions. This library is located into the folder pointed by the BSHM variable. Please set this in your ".bashrc". My .bashrc file has the following lines to set this:

    ~ Begin .bashrc segment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ do not copy this line ~
    ## Tomo
    export BSHM="${HOME}/bin/BSHM"
    export GMTDATA="${HOME}/bin/gmt.mapa/"
    mkdir -p "$BSHM" "$GMTDATA"
    ~ end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ do not copy this line ~

When you have this working, open a new terminal, go to the folder tomocode/SHELL/ and run:

    % bash tomo.bshm install

This will make a link from $BSHM/tomo.bshm to tomocode/SHELL/tomo.bshm

3) In your shell load the module. 

    % source $BSHM/tomo.bshm

4) Now you can use the function "tomo". Just type in "tomo" for getting a help notice.

Bianchi ~ 24-01-2014

