!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : DOT_PRODUCT intrinsic
!*
!* DESCRIPTION                : array constructor of integer and
!*                              logical types
!* ===================================================================

integer(1) :: i1=dot_product((/-4_1, 4_1/), (/2_1, -30_1/))
integer(2) :: i2=dot_product((/71_2, 2_2/), (/32_2, 31_2/))
integer(4) :: i4=dot_product((/1_4, 65536_4/), (/2_4, 3_4/))
integer(8) :: i8=dot_product((/1_8, 2_8/), (/2_8, 1073741824_8/))

logical(1) :: l1=dot_product((/.true._1, .false._1/), (/.false._1, .false._1/))
logical(2) :: l2=dot_product((/.true._2, .true._2/), (/.true._2, .true._2/))
logical(4) :: l4=dot_product((/.true._4, .false._4/), (/.false._4, .true._4/))
logical(8) :: l8=dot_product((/.true._8, .true._8/), (/.true._8, .true._8/))

if (l1 .neqv. dot_product((/.true._1, .false._1/), &
  & (/.false._1, .false._1/))) error stop 1
if (l2 .neqv. dot_product((/.true._2, .true._2/), &
  & (/.true._2, .true._2/))) error stop 2
if (l4 .neqv. dot_product((/.true._4, .false._4/), &
  & (/.false._4, .true._4/))) error stop 3
if (l8 .neqv. dot_product((/.true._8, .true._8/), &
  & (/.true._8, .true._8/))) error stop 4

if (i1 .ne. dot_product((/-4_1, -4_1/), (/2_1, 30_1/))) error stop 5
if (i1 .ne. -128) error stop 55
if (i2 .ne. dot_product((/71_2, 2_2/), (/32_2, 31_2/))) error stop 6
if (i4 .ne. dot_product((/1_4, 65536_4/), (/2_4, 3_4/))) error stop 7
if (i8 .ne. dot_product((/1_8, 2_8/), (/2_8, 1073741824_8/))) error stop 8
end
