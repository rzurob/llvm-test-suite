!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2014-04-07
!*
!*  PRIMARY FUNCTIONS TESTED   : FINDLOC intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that findloc RTE function returns
!*                               0 when VALUE is not found.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
program findloc_notfound
implicit none

integer, parameter :: arr(*) = [1, 4, 3, 4, 5]
integer, parameter :: arr2c(-3:1) = arr
integer, parameter :: arr2dc(3,4) = reshape([1, 2, 3, 4, 5, 3, 6, 7, 3, 8, 9, 9], [3,4])
integer, parameter :: arr3dc(2,4,3) = &
  reshape ([1,2,3,4,5,6,3,7,8,9,10,11,12,13,14,3,15,16,17,18,19,20,3,21], [2,4,3])

logical, parameter :: m(*) = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.]
logical, parameter :: m2d(*,*) = &
  reshape ([.TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE., &
            .TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE.], shape(arr2dc))
logical, parameter :: m3d(*,*,*) = &
  reshape([.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  .TRUE., &
           .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  .TRUE.,  &
           .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE.], shape(arr3dc))

! Rank = 1, search forward
integer, parameter :: res_nf_a_v(*) = findloc(arr, -99)
integer, parameter :: res_nf_a_v_1 = findloc(arr, -99, 1)
integer, parameter :: res_nf_a_v_m(*) = findloc(arr, -99, m)

! Rank = 1, search backward
integer, parameter :: res_nf_a_v_b(*) = findloc(arr, -99, back=.true.)
integer, parameter :: res_nf_a_v_1_b = findloc(arr, -99, 1, back=.true.)
integer, parameter :: res_nf_a_v_m_b(*) = findloc(arr, -99, m, back=.true.)

! Rank = 1, search forward
integer, parameter :: res_nf_ac_v(*) = findloc(arr2c, -99)
integer, parameter :: res_nf_ac_v_1 = findloc(arr2c, -99, 1)
integer, parameter :: res_nf_ac_v_m(*) = findloc(arr2c, -99, m)

! Rank = 1, search backward
integer, parameter :: res_nf_ac_v_b(*) = findloc(arr2c, -99, back=.true.)
integer, parameter :: res_nf_ac_v_1_b = findloc(arr2c, -99, 1, back=.true.)
integer, parameter :: res_nf_ac_v_m_b(*) = findloc(arr2c, -99, m, back=.true.)

! Rank = 2, search forward
integer, parameter :: res_nf_a2_v(*) = findloc(arr2dc, -99)
integer, parameter :: res_nf_a2_v_1(*) = findloc(arr2dc, -99, 1)
integer, parameter :: res_nf_a2_v_2(*) = findloc(arr2dc, -99, 2)
integer, parameter :: res_nf_a2_v_m(*) = findloc(arr2dc, -99, m2d)

! Rank = 2, search backward
integer, parameter :: res_nf_a2_v_b(*) = findloc(arr2dc, -99, back=.true.)
integer, parameter :: res_nf_a2_v_1_b(*) = findloc(arr2dc, -99, 1, back=.true.)
integer, parameter :: res_nf_a2_v_2_b(*) = findloc(arr2dc, -99, 2, back=.true.)
integer, parameter :: res_nf_a2_v_m_b(*) = findloc(arr2dc, -99, m2d, back=.true.)

! Rank = 3, search forward
integer, parameter :: res_nf_a3_v(*) = findloc(arr3dc, -99)
integer, parameter :: res_nf_a3_v_1(*, *) = findloc(arr3dc, -99, 1)
integer, parameter :: res_nf_a3_v_2(*, *) = findloc(arr3dc, -99, 2)
integer, parameter :: res_nf_a3_v_3(*, *) = findloc(arr3dc, -99, 3)
integer, parameter :: res_nf_a3_v_m(*) = findloc(arr3dc, -99, m3d)

! Rank = 3, search backward
integer, parameter :: res_nf_a3_v_b(*) = findloc(arr3dc, -99, back=.true.)
integer, parameter :: res_nf_a3_v_1_b(*, *) = findloc(arr3dc, -99, 1, back=.true.)
integer, parameter :: res_nf_a3_v_2_b(*, *) = findloc(arr3dc, -99, 2, back=.true.)
integer, parameter :: res_nf_a3_v_3_b(*, *) = findloc(arr3dc, -99, 3, back=.true.)
integer, parameter :: res_nf_a3_v_m_b(*) = findloc(arr3dc, -99, m3d, back=.true.)

if (ANY(res_nf_a_v /= [0])) ERROR STOP 1
if (res_nf_a_v_1 /= 0) ERROR STOP 2
if (ANY(res_nf_a_v_m /= [0])) ERROR STOP 3
if (ANY(res_nf_a_v_b /= [0])) ERROR STOP 4
if (res_nf_a_v_1_b /= 0) ERROR STOP 5
if (ANY(res_nf_a_v_m_b /= [0])) ERROR STOP 6

if (ANY(res_nf_ac_v /= [0])) ERROR STOP 7
if (res_nf_ac_v_1 /= 0) ERROR STOP 8
if (ANY(res_nf_ac_v_m /= [0])) ERROR STOP 9
if (ANY(res_nf_ac_v_b /= [0])) ERROR STOP 10
if (res_nf_ac_v_1_b /= 0) ERROR STOP 11
if (ANY(res_nf_ac_v_m_b /= [0])) ERROR STOP 12

if (ANY(res_nf_a2_v /= [0, 0])) ERROR STOP 13
if (ANY(res_nf_a2_v_1 /= reshape([0, 0, 0, 0], [4]))) ERROR STOP 14
if (ANY(res_nf_a2_v_2 /= reshape([0, 0, 0], [3]))) ERROR STOP 15
if (ANY(res_nf_a2_v_m /= [0, 0])) ERROR STOP 16

if (ANY(res_nf_a2_v_b /= [0, 0])) ERROR STOP 17
if (ANY(res_nf_a2_v_1_b /= reshape([0, 0, 0, 0], [4]))) ERROR STOP 18
if (ANY(res_nf_a2_v_2_b /= reshape([0, 0, 0], [3]))) ERROR STOP 19
if (ANY(res_nf_a2_v_m_b /= [0, 0])) ERROR STOP 20

if (ANY(res_nf_a3_v /= [0, 0, 0])) ERROR STOP 21
if (ANY(res_nf_a3_v_1 /= reshape([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [4,3]))) ERROR STOP 22
if (ANY(res_nf_a3_v_2 /= reshape([0, 0, 0, 0, 0, 0], [2,3]))) ERROR STOP 23
if (ANY(res_nf_a3_v_3 /= reshape([0, 0, 0, 0, 0, 0, 0, 0], [2,4]))) ERROR STOP 24
if (ANY(res_nf_a3_v_m /= [0, 0, 0])) ERROR STOP 25

if (ANY(res_nf_a3_v_b /= [0, 0, 0])) ERROR STOP 26
if (ANY(res_nf_a3_v_1_b /= reshape([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [4,3]))) ERROR STOP 27
if (ANY(res_nf_a3_v_2_b /= reshape([0, 0, 0, 0, 0, 0], [2,3]))) ERROR STOP 28
if (ANY(res_nf_a3_v_3_b /= reshape([0, 0, 0, 0, 0, 0, 0, 0], [2,4]))) ERROR STOP 29
if (ANY(res_nf_a3_v_m_b /= [0, 0, 0])) ERROR STOP 30

end program
