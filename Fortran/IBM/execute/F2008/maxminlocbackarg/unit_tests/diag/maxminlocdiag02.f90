! Calls to maxloc/minloc with non-scalar BACK argument

integer :: j1(1) = maxloc((/1_1, 3_1, 3_1, 2_1/), BACK = (/.true., .true., .true.,.true./))
integer :: j2(1)

j2 = minloc((/1_1, 3_1, 3_1, 2_1/), BACK = (/.true., .false./))

end
