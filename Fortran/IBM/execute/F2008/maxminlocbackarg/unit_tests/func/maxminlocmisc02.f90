! Calls to maxloc and minloc where back is in a different position.

implicit none

integer :: j1(1)
integer :: j2(2), j3(2), j4(2)

j1 = maxloc((/1_1, 3_1, 3_1, 2_1/), back = .true., mask = (/.true., .true., .false., .true./))
j2 = maxloc(reshape((/1_1, 3_1, 3_1, 2_1/), (/2,2/)), back = .true., dim = 1)
j3 = minloc(reshape((/3_1, 1_1, 2_1, 1_1/), (/2,2/)), 2, back = .false., mask = .true.)
j4 = minloc(reshape((/3_1, 1_1, 1_1, 2_1/), (/2,2/)), kind = 4, back = .true.)

if (j1(1) .ne. 2) ERROR STOP 1
if (j2(1) .ne. 2 .or. j2(2) .ne. 1) ERROR STOP 2
if (j3(1) .ne. 2 .or. j3(2) .ne. 1) ERROR STOP 3
if (j4(1) .ne. 1 .or. j4(2) .ne. 2) ERROR STOP 4

end
