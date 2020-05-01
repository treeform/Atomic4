# Atomic4 - May 10, 2004

This Os is one of the smallest there is. I wrote it part as a joke part as an exercise test for my skill.

About 2 month ago (May 10, 2004), I was browsing the Internet and then on OsDev I saw an interesting project called BS/OS. It stated this OS is one boot sector big. And then it stated that it is a crazy idea. Well and I like crazy ideas. So I decided to write the world's tiniest full functioning OS. Which is the craziest thing I have heard!

Atomic 2: I have so many ideas but this year's finals ate up all my wind.

World is full of crazy ideas. A week ago Asko decided to help me some out with AtomicOS so he really streamlined it. Now it is smaller, uses the upcodes much better. Yet space is there to be filled so I wrote a memory manager for this Atomic version 3.  Now we come to the problem of fully utilizing the OS.  For release 4 clock and calculator were added.  Plus to flip through the memory better the up command was created.

Yet Asko produces lots of code and he already has made the DebugOs.

Code:  I have been coding on and off. So I have written a little OF that loads and saves files (those were real files) and then one with memory management and then another that is just a GUI with a couple of buttons. So I had some code left over. I reused the code. So it went fast, just copy paste.
User Tools:  Then I wanted to add a little compiler. Now since I realized it would take more space then just 512 bytes. So now I am in the process of adding an interpreter. But before I achieve an interpreter I wanted to make a prompt like a calculator. But then Asko emailed me a debugger since then I have made significant progress in the OS! So Atomic 3 has to come out just with a debugger. I am shore Asko or I will make many more programs since so many of them are in the design.
For User:  The shell has 7 commands


-------------------------------------------

# Commands:

* `Load` #Sector loads the sector
* `Save` #Sector saves the sector
* `Edit` lets you input the sector
* `Type` lets you display what you loaded
* `Run` runs the program that was loaded
* `New` Allocates new memory
* `Mem` displays the memory table
* `Up` goes to the next allocated page.
-------------------------------------------

# Tutorial:

Hands on: Ok you started the OS here is a little guide.

Type: `Load 0`

You have loaded the boot sector where the os is

Type `Type` (it does not display well because it is not text the first 0 encountered makes stops the printing )

Type: `Load 1`

You have loaded the help file

Type again and it shows you the help file

Now type `Edit`

Then type a little note for yourself (Like: " I wrote this")

After you finish, Save it by typing `Save 500`

For our 500 - any number will do as long as it is > 10 (because System files are at 0-10)

Now load the boot sector again (`Load 0`)

Now load sector `Load 500`

Type and You must see your little note!

Now how about run a program

Let choose the clock
Type `Load 2`

Now you have loaded system clock written by Asko

Type in `Run`

Now you see the time in the corner of the monitor

Now let's do the calculator!

`Load 4`

`Type` the little help file

`Load 3` the calculator it self

`Run` it

Type something like 2-2-2-2-2-2-2-2-2 and it will tell you that it is -16.

Use `x` for exit

Now the debugger

`Load 5` and run

Use it at your will.

That's it on the OS. This OS is a joke right? Fits on a boot sector, files that are sectors, help files like they really mean anything, and tiny Os in a big Os developing community with many realities...

----------------------------------

To run this with bochs:

```
bochs.exe 'boot:floppy' 'floppya: 1_44=boot.img, status=inserted'
```
