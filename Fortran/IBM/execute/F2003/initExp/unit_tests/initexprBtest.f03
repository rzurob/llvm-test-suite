!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : BTEST intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

logical(1) :: l1=btest(117_1, 3), l1res
logical(2) :: l2=btest(71_2, 2), l2res
logical(4) :: l4=btest(321_4, 8), l4res
logical(8) :: l8=btest(123_8, 36), l8res

l1res = btest(117_1, 3)
l2res = btest(71_2, 2)
l4res = btest(321_4, 8)
l8res = btest(123_8, 36)

if (l1 .neqv. l1res) error stop 1
if (l2 .neqv. l2res) error stop 2
if (l4 .neqv. l4res) error stop 4
if (l8 .neqv. l8res) error stop 8
end