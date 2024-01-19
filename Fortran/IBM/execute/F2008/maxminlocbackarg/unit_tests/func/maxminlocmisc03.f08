! Calls to maxloc and minloc with -qintlog set.

integer :: j1(1) = maxloc((/1_1, 3_1, 3_1, 2_1/), back = 2)
integer :: j2(1) = maxloc((/1_1, 3_1, 3_1, 2_1/), back = 1)
integer :: j3(1), j4(1)

j3 = minloc((/1_1, 3_1, 1_1, 2_1/), back = 4)
j4 = minloc((/1_1, 3_1, 1_1, 2_1/), back = 3)

if (j1(1) .ne. 2) ERROR STOP 1
if (j2(1) .ne. 3) ERROR STOP 2
if (j3(1) .ne. 1) ERROR STOP 3
if (j4(1) .ne. 3) ERROR STOP 4

end
