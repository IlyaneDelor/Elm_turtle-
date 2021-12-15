Primary Feature :

✔️support the forward 10 command which makes the turtle advance by 10 units (up to you to define what a handy "unit" would be: px? 100px?). Note: of course, the 10 can vary, the user may enter any integer.

✔️support the pen_up and pen_down instructions. When the pen is "up", the turtle moves without drawing anything behind it.

✔️support turn_left 20 and turn_right 20 which makes the turtle turn on it self by 20 degrees (once again, this 20 may actually be any integer).

✔️change the color: color red (you can define the set of color you like).

✔️clean the screen without changing turtle position: clean.



Optional features

repeat commands multiple times: repeat 4 [ forward 10, turn_left 90 ] (this draws a square!).

make it possible to define some numeric variables: var x = 8. With, we can easily define polygons: var x = 10 repeat x [ forward 10, turn_left 360 / x]

make it possible to define some procedures: proc square = repeat 4 [ forward 10, turn_left 90 ] end Then we can invoke this procedure by just writing square.

add some parameters to the procedures: proc square length = repeat 4 [ forward length, turn_left 90 ]

make it possible for the user to save the current session as a file and to restore the session by loading the file. Hint: the whole state of the session only depends on the user commands.
