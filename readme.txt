This Os is one of the smallest there is. I wrote it part as a joke part as an exercise a test for my skill. 

About 2 month ago (May 10, 2004), I was browsing the Internet and then I on OsDev I saw and interesting project called BS/OS. It stated this OS is one boot sector big. And then it stated that it is a crazy idea. Well and I like crazy ideas. So I decided to write the world most tiniest full functioning OS. Which is the craziest thing I have heard! 

Atomic 2: I have so many ideas but this year's finals eaten up all my wind. 

World is full of crazy ideas. A week ago Asko decided to help me some out with AtomicOS so he really streamed lined it. Now it is smaller, uses the upcodes much better. Yet space is there to be filled so I wrote a memory manager of for this Atomic version 3.  Now we come to problem of fully utilizing the OS.  For release 4 clock and calculator where added.  Plus to flip through the memory better the up command was created.

Yet Asko produces lots of code and he already has made the DebugOs. 

 
Code:  I have been coding on and off. So I have written a little OS that loads and saves files (those where real files) and then one with memory management and then other that is just a GUE with couple of buttons. So I had some code left over. I reused the code. So it whent fast, just copy paste.  
User Tools:  Then I wanted to add little compiler. Now since I realized it would take more space then just 512 bytes. So now I am in a process of an interpreter. But before I achieve an interpreter I wanted to make a prompt like calculator. But then Asko emailed me a debugger since then I have made significant progress in the OS! So Atomic 3 has to come out just with a debugger. I am shore Asko or I will make many more programs since so many of them are in the design.  
For User:  The shell has 7 commands


-------------------------------------------
Load [Sector] loads the secto

Save [Sector] saves the sector

Edit lets you input the sector

Type lets you disply what you loaded

Run runs the program that was loaded

New Alocates new memory

Mem displays the memory table

Up goes to the next allocated page.
-------------------------------------------



Hands on:  Ok you started the OS here is a little guide. 

Type: Load 0

You have loaded the boot sector where the os is 

Type "Type" (it does not display well because it is not text the first 0 encountered makes stops the printing ) 

Type: Load 1

You have loaded the help file

Type again and it shows you the help file

Now type Edit

Then type a little note for your self (Like: " I wrote this")

After you finish, Save it by typing Save 500

For our 500 - any number will do as long as it is > 10 (because System files are at 0-10)

Now load the boot sector again (Load 0)

Now load Sector 500

Type and You must see your little note!

Now how about run a program

Let chouse the clock
Type Load 2

Now you have loaded system clock written by Asko

Type in Run 

Now you see the time in the corner of the monitor

Now lets do the calculator!  

Load 4

Type the little help file

Load 3 the calculator it self

Run it

Type something like 2-2-2-2-2-2-2-2-2 and it will say you that it is -16.

Use x for exit

Now the debugger 

Load 5 and run

Use it at your will.

That's it of the OS. This OS is a joke right? Fits on a boot sector, files that are sectors, help files like they really mean anything, and tiny Os in a big Os developing community with many realities... Thanks for visit Andre 

I will take any code or suggestions that you have to contribute (Yes, I am hungry for help) 

Email: eiforall@hotmail.com 
