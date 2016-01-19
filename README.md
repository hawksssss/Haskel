General Setup (v0.1.1)
======================


Welcome to CS 421! Before we can get started, you'll need to set up your
environment with at least both git and Haskell. (We'll cover the latter later.)

Installing git
--------------

If you haven't yet, you should setup [Git](https://git-scm.com/book/en).
Extensive install guides exist online (even at the linked to book), but we've
provided brief instructions here.

### Windows (CLI)

1.  Download the executable here: https://git-scm.com/download/win
1.  Follow the instructions to install and set it up.

### Mac OS X (CLI)

1.  Install Homebrew: http://brew.sh/
1.  After installing Homebrew, fire up a terminal and run `brew install git`
1.  Follow the instructions given.

### Linux (CLI)

Linux is distribution specific, the commands to run for several popular distros
is shown below.

-   Debian: `sudo apt-get install git-core`
-   Ubuntu: `sudo apt-get install git`
-   Arch: `sudo pacman -S git`
-   Gentoo: `sudo emerge --ask git`
-   Fedora: `sudo yum install git-all`

Setup git
---------

More concretely:

1.  Open up a Terminal/your git executable.
1.  Modify the following two lines.
1.  Execute them!

    ```sh
     git config --global user.name "YOUR NAME"
     git config --global user.email "NETID@illinois.edu"
     ```

Now, you'll want to navigate (in your Terminal/git executable) to the place you
want your repository to reside.

Now that you've done that, you'll want to grab a copy of your repository. How do
you do this? Well, by going:

```sh
git clone git@gitlab-beta.engr.illinois.edu:cs421-sp2016/NETID.git
```

You can safely move or rename the folder it creates (initially called "NETID")
around, as long as you don't move or rename the `.git` directory inside it. Just
remember where you put it - this will be how you submit homework to us.
