! Calls to maxloc and minloc where back has a runtime value.

implicit none

integer :: j1(1), j2(1)
integer :: j3(2), j4(2)
integer :: j5(3), j6(3)

logical :: back1, back2

back1 = .true.
back2 = .false.

j1 = maxloc((/1_1, 3_1, 3_1, 2_1/), back = back1)
j2 = maxloc((/1_1, 3_1, 3_1, 2_1/), back = back2)
j3 = minloc(reshape((/3_1, 1_1, 1_1, 2_1/), (/2,2/)), back = back1)
j4 = minloc(reshape((/3_1, 1_1, 1_1, 2_1/), (/2,2/)), back = back2)
j5 = minloc(reshape((/1_1, 3_1, 4_1, 2_1, 1_1, 3_1, 4_1, 2_1/), (/2,2,2/)), back = back1)
j6 = maxloc(reshape((/1_1, 3_1, 4_1, 2_1, 1_1, 3_1, 4_1, 2_1/), (/2,2,2/)), back = back2)

if (j1(1) .ne. 3) ERROR STOP 1
if (j2(1) .ne. 2) ERROR STOP 2
if (j3(1) .ne. 1 .or. j3(2) .ne. 2) ERROR STOP 3
if (j4(1) .ne. 2 .or. j4(2) .ne. 1) ERROR STOP 4
if (j5(1) .ne. 1 .or. j5(2) .ne. 1 .or. j5(3) .ne. 2) ERROR STOP 5
if (j6(1) .ne. 1 .or. j6(2) .ne. 2 .or. j6(3) .ne. 1) ERROR STOP 6

end
