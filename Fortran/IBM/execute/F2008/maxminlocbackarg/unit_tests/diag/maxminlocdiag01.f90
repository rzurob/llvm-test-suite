! Calls to maxloc/minloc with BACK argument in wrong position

integer :: j1(1) = maxloc((/1_1, 3_1, 3_1, 2_1/), (/.true., .true., .true.,.true./), .true.)
integer :: j2(1)

j2 = minloc((/1_1, 3_1, 3_1, 2_1/), .true., .false.)

end
