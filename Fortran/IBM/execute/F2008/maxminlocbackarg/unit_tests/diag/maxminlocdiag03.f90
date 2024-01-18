! Calls to maxloc/minloc with incorrect type of BACK argument

integer :: j1(1) = maxloc((/1_1, 3_1, 3_1, 2_1/), back = 'a')
integer :: j2(1)

j2 = minloc((/1_1, 3_1, 3_1, 2_1/), BACK = 4.0)

end
