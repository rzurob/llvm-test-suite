!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-05-27
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FINDLOC intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FINDLOC (ARRAY, VALUE, DIM [, MASK, KIND, BACK])
!*                               FINDLOC (ARRAY, VALUE [, MASK, KIND, BACK])
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM  FINDLOC_INTEGER
implicit none

!-- test when findloc() appears in a constant expression --!
integer, parameter :: loc =  findloc([1,2,3,4,5], 2, 1)

!-- all values are known at compile time:
integer, parameter :: kind1=4
logical, parameter :: b=.TRUE.
integer, parameter :: arr(*) = [1, 4, 3, 4, 5], val = 4, d = 1
integer, parameter :: arr2c(-3:1) = arr
integer, parameter :: arr2dc(3,4) = reshape([1, 2, 3, 4, 5, 3, 6, 7, 3, 8, 9, 9], [3,4])
integer, parameter :: arr3dc(2,4,3) = &
  reshape ([1,2,3,4,5,6,3,7,8,9,10,11,12,13,14,3,15,16,17,18,19,20,3,21], [2,4,3])

logical, parameter :: s_m = .true.
logical, parameter :: m(*) = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.]
logical, parameter :: m1(*) =    [.TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.]
logical, parameter :: m2(*) =    [.FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.]
logical, parameter :: m3(-2:2) = [.TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.]
logical, parameter :: m4(*) =    [.FALSE., .FALSE., .FALSE., .FALSE., .FALSE.]
logical, parameter :: m2d(*,*) = &
  reshape ([.TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE., &
            .TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE.], shape(arr2dc))
logical, parameter :: m3d(*,*,*) = &
  reshape([.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  .TRUE., &
           .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  .TRUE.,  &
           .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE.], shape(arr3dc))

integer, parameter :: res_a_v(*) = findloc(arr, val)
integer, parameter :: res_a_v_d = findloc(arr, val, d)
integer(kind1), parameter :: res_a_v_m_k_b(*) = findloc(arr, val, m, kind1, b)
integer, parameter :: res_a_v_m_b(*) = findloc(arr, val, m, BACK=b)
integer, parameter :: res_a_4_m_f(*) = findloc(arr, 4, Mask=m1, BACK=.false.)
integer(4), parameter :: res_a_4_1_m_4_f = findloc(arr, 4, 1, m1, 4, .false.)
integer(4), parameter :: res_a_4_1_m_4_t = findloc(arr, 4, 1, m1, 4, .true.)
integer(4), parameter :: res_v_a_f_4_m_1 = findloc(VALUE=4, ARRAY=arr, BACK=.false., KIND=4, MASK=m1, DIM=1)
integer, parameter :: k_res_a_4_1_m_1_t = kind(findloc(arr, 4, 1, m1, 1, .true.))
integer, parameter :: k_res_a_4_1_m_2_t = kind(findloc(arr, 4, 1, m1, 2, .true.))
integer, parameter :: k_res_a_4_1_m_t = kind(findloc(arr, 4, 1, m1, BACK=.true.))
integer, parameter :: k_res_a_4_1_m_8_t = kind(findloc(arr, 4, 1, m1, 8, .true.))
integer, parameter :: res_a_4_1_m_t = findloc(arr, 4, 1, MASK=m1, BACK=.TRUE.)
integer(4), parameter :: res_a_4_m_4_f(*) = findloc(arr, 4, m1, 4, .false.)
integer, parameter :: res_a2_3(*) = findloc(arr2dc, 3)
integer, parameter :: res_a2_3_m(*) = findloc(arr2dc, 3, m2d)
integer, parameter :: res_a2_3_sm(*) = findloc(arr2dc, 3, s_m)
integer, parameter :: res_a2_3_1(*) = findloc(arr2dc, 3, 1)
integer, parameter :: res_a2_3_2(*) = findloc(arr2dc, 3, 2)
integer, parameter :: res_a2_3_1_m(*) = findloc(arr2dc, 3, 1, m2d)
integer, parameter :: res_a2_3_2_m(*) = findloc(arr2dc, 3, 2, m2d)
integer(2), parameter :: res_a2_3_1_m_2(*) = findloc(arr2dc, 3, 1, m2d, 2)
integer, parameter :: res_a2_3_t(*) = findloc(arr2dc, 3, BACK=.TRUE.)
integer, parameter :: res_a2_3_m_t(*) = findloc(arr2dc, 3, m2d, BACK=.TRUE.)
integer, parameter :: res_a2_9_1(*) = findloc(arr2dc, 9, 1)
integer, parameter :: res_a2_9_1_f(*) = findloc(arr2dc, 9, 1, BACK=.FALSE.)
integer, parameter :: res_a2_9_1_t(*) = findloc(arr2dc, 9, 1, BACK=.TRUE.)
integer, parameter :: res_a3_3(*) = findloc(arr3dc, 3)
integer, parameter :: res_a3_3_1(*,*) = findloc(arr3dc, 3, 1)
integer, parameter :: res_a3_3_2(*,*) = findloc(arr3dc, 3, 2)
integer, parameter :: res_a3_3_3(*,*) = findloc(arr3dc, 3, 3)
integer, parameter :: res_a3_3_m_t(*) = findloc(arr3dc, 3, m3d, BACK=.TRUE.)
integer, parameter :: res_a_6_1 = findloc(arr, 6, 1)
integer, parameter :: res_a_6_1_m = findloc(arr, 6, 1, m4)


!-- in the following cases FE does not know the values @ compile time:
integer  :: arr1(5), arr2(-3:1), arr2d(3,4), arr3d(2,4,3), arr0(5:-3)
logical  :: mask1(5), mask2(5), mask3(-2:2), mask4(5),  s_mask, back1, mask2d(3,4), mask3d(2,4,3)
integer :: val1, dim1, val_input


arr1 = [1, 4, 3, 4, 5]
arr2 = arr1
arr2d = reshape([1, 2, 3, 4, 5, 3, 6, 7, 3, 8, 9, 9], [3,4])
arr3d = reshape ([1,2,3,4,5,6,3,7,8,9,10,11,12,13,14,3,15,16,17,18,19,20,3,21], [2,4,3])
arr0 = reshape([1,2,3], shape(arr0))
val1 = 4
dim1 = 1

back1 = .TRUE.

s_mask = .true.
mask1 = [.TRUE., .TRUE., .TRUE. ,.TRUE., .TRUE.]
mask2 = [.FALSE., .TRUE., .TRUE., .TRUE., .TRUE.]
mask3 = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.]
mask4 = [.FALSE.,.FALSE., .FALSE., .FALSE., .FALSE.]
mask2d = reshape ([.TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .FALSE., .TRUE., .TRUE., .TRUE.], shape(arr2d))
mask3d = reshape([.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE.], shape(arr3d))

if (res_a_v_d /= 2) ERROR STOP 111
if (ANY(res_a_4_m_f /= [2])) ERROR STOP 115
if (res_a_4_1_m_4_f /= 2) ERROR STOP 116
if (res_a_4_1_m_4_t /= 4) ERROR STOP 117
if (res_v_a_f_4_m_1 /= 2) ERROR STOP 118
if (k_res_a_4_1_m_1_t /= 1) ERROR STOP 119
if (k_res_a_4_1_m_2_t /= 2) ERROR STOP 120
if (k_res_a_4_1_m_t /= 4) ERROR STOP 121
if (k_res_a_4_1_m_8_t /= 8) ERROR STOP 122
if (res_a_4_1_m_t /= 4) ERROR STOP 123
if (ANY(res_a_4_m_4_f /= [2])) ERROR STOP 124
if (ANY(res_a2_3 /= [3,1])) ERROR STOP 134
if (ANY(res_a2_3_m /= [3,2])) ERROR STOP 135
if (ANY(res_a2_3_sm /= [3,1])) ERROR STOP 136
if (ANY(res_a2_3_1 /= [3,3,3,0])) ERROR STOP 137
if (ANY(res_a2_3_2 /= [0,0,1])) ERROR STOP 138
if (ANY(res_a2_3_1_m /= [0,3,0,0])) ERROR STOP 139
if (ANY(res_a2_3_2_m /= [0,0,2])) ERROR STOP 140
if (ANY(res_a2_3_1_m_2 /= [0,3,0,0])) ERROR STOP 141
if (ANY(res_a2_3_t /= [3,3])) ERROR STOP 142
if (ANY(res_a2_3_m_t /= [3,2])) ERROR STOP 143
if (ANY(res_a2_9_1 /= [0,0,0,2])) ERROR STOP 144
if (ANY(res_a2_9_1_f /= [0,0,0,2])) ERROR STOP 145
if (ANY(res_a2_9_1_t /= [0,0,0,3])) ERROR STOP 146
if (ANY(res_a3_3 /= [1,2,1])) ERROR STOP 147
if (ANY(res_a3_3_1 /= reshape([0,1,0,1,0,0,0,2,0,0,0,1],[4,3]))) ERROR STOP 148
if (ANY(res_a3_3_2 /= reshape([2,0,0,4,4,0],[2,3]))) ERROR STOP 149
if (ANY(res_a3_3_3 /= reshape([0,0,1,0,0,0,1,2],[2,4]))) ERROR STOP  150
if (ANY(res_a3_3_m_t /= [2,4,2])) ERROR STOP 151
if (res_a_6_1 /= 0) ERROR STOP 153
if (res_a_6_1_m /= 0) ERROR STOP 154

!-- comparing the result when some of the arguments are known at compile time vs all of them vs none of them:
if (ANY(res_a_v /= findloc(arr1, val1))) ERROR STOP 10
!-- RTE should fix dim problem
if (res_a_v_d /= findloc(arr1, val1, dim1)) ERROR STOP 11
if (ANY(findloc(arr, val1) /= findloc(arr1, val))) ERROR STOP 12
if (ANY(res_a_v_m_k_b /= findloc(arr1, val1, mask1, kind1, back1))) ERROR STOP 13
if (ANY(res_a_v_m_b /= findloc([1, 4, 3, 4, 5], 4, &
                                [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.],&
                                BACK=.TRUE.))) ERROR STOP 14


if (ANY(findloc(arr1, 4, Mask=mask1, BACK = .false.) /= [2])) ERROR STOP 15

if (findloc(arr1, 4, 1, mask1, 4, .false.) /= 2) ERROR STOP 16

!--  when all args are present:
if (findloc(arr1, 4, 1, mask1, 4, .true.) /= 4) ERROR STOP 17
!-- using the argument keywords:
if (findloc(VALUE=4, ARRAY=arr1, BACK=.false., KIND=4, MASK=mask1, DIM=1) /= 2) ERROR STOP 18

!-- with different kind arg:
if (kind(findloc(arr1, 4, 1, mask1, 1, .true.)) /= 1) ERROR STOP 19
if (kind(findloc(arr1, 4, 1, mask1, 2, .true.)) /= 2) ERROR STOP 20
if (kind(findloc(arr1, 4, 1, mask1, BACK=.true.)) /= 4) ERROR STOP 21
if (kind(findloc(arr1, 4, 1, mask1, 8, .true.)) /= 8) ERROR STOP 22

!-- when KIND argument is absent:
if (findloc(arr1, 4, 1, MASK=mask1, BACK = .TRUE.) /= 4) ERROR STOP 23

!-- when DIM argument is absent,the result is an array of rank one and of size equal to the rank of arr1:
if (ANY(findloc(arr1, 4, mask1, 4, .false.) /= [2])) ERROR STOP 24

!-- when MASK argument is absent:
 ! for now it does not work becasue of dim1. RTE should fix it!
if (findloc(arr1, val1, dim1, KIND=kind1,BACK=.TRUE.) /= 4) ERROR STOP 25

if (findloc(arr1, val1, 1, KIND=kind1, BACK=.true.) /= 4) ERROR STOP 26

!-- if MASK argument is present and some of its elemts are false versus a mask with all true elements:
if (findloc(arr1, 1, 1, mask1) /= 1) ERROR STOP 27
if (findloc(arr1, 1, 1, mask2) /= 0) ERROR STOP 28

!-- when BACK argument is absent:
if (findloc(arr1, val1, 1, mask1, kind1) /= findloc(arr1, val1, 1, mask1, kind1, .false.)) ERROR STOP 29

!-- when MASK is a scalar
if (findloc(arr1, val1, 1, mask1, kind1) /= findloc(arr1, val1, 1, s_mask, kind1)) ERROR STOP 30

!-- test with different lower bounds of ARRAY/MASK argument:
if (findloc(arr1, val1, 1, mask1) /= findloc(arr2, val1, 1, mask1)) ERROR STOP 31
if (findloc(arr1, val1, 1) /= findloc(arr2, val1, 1)) ERROR STOP 32
if (findloc(arr1, val1, 1, mask3) /= findloc(arr2, val1, 1, mask1)) ERROR STOP 33

!-- test with 2-dimensional array:
if (ANY(findloc(arr2d, 3) /= [3,1])) ERROR STOP 34
if (ANY(findloc(arr2d, 3, mask2d) /= [3,2])) ERROR STOP 35
if (ANY(findloc(arr2d, 3, s_mask) /= [3,1])) ERROR STOP 36

if (ANY(findloc(arr2d, 3, 1) /= [3,3,3,0])) ERROR STOP 37
if (ANY(findloc(arr2d, 3, 2) /= [0,0,1])) ERROR STOP 38

if (ANY(findloc(arr2d, 3, 1, mask2d) /= [0,3,0,0])) ERROR STOP 39
if (ANY(findloc(arr2d, 3, 2, mask2d) /= [0,0,2])) ERROR STOP 40


if (ANY(findloc(arr2d, 3, 1, mask2d, 2) /= [0,3,0,0])) ERROR STOP 41

if (ANY(findloc(arr2d, 3, BACK=.TRUE.) /= [3,3])) ERROR STOP 42
if (ANY(findloc(arr2d, 3, mask2d, BACK=.TRUE.) /= [3,2])) ERROR STOP 43

if (ANY(findloc(arr2d, 9, 1) /= [0,0,0,2])) ERROR STOP 44
if (ANY(findloc(arr2d, 9, 1, BACK=.FALSE.) /= [0,0,0,2])) ERROR STOP 45
if (ANY(findloc(arr2d, 9, 1, BACK=.TRUE.) /= [0,0,0,3])) ERROR STOP 46

!-- test with 3-dimensional array:
if (ANY(findloc(arr3d, 3) /= [1,2,1])) ERROR STOP 47

if (ANY(findloc(arr3d, 3, 1) /= reshape([0,1,0,1,0,0,0,2,0,0,0,1],[4,3]))) ERROR STOP 48
if (ANY(findloc(arr3d, 3, 2) /= reshape([2,0,0,4,4,0],[2,3]))) ERROR STOP 49
if (ANY(findloc(arr3d, 3, 3) /= reshape([0,0,1,0,0,0,1,2],[2,4]))) ERROR STOP  50

if (ANY(findloc(arr3d, 3, mask3d, BACK=.TRUE.) /= [2,4,2])) ERROR STOP 51

!-- check the result of findloc in a constant expression --!
if (loc /= 2) ERROR STOP 52

!-- when no matching value is found --!
if (findloc(arr1, 6, 1) /= 0) ERROR STOP 53

if (findloc(arr1, 6, 1, mask4) /= 0) ERROR STOP 54

if (findloc(arr0, 6, 1) /= 0) ERROR STOP 55

!-- test when findloc is used as an actual argument to another function --!
if (minval(findloc(arr1, 4)) /= 2) ERROR STOP 56

!-- test when the argument of findloc is the result of a function --!
if (findloc([1,2,3,4,24], product([1,2,3,4]), 1 ) /= 5) ERROR STOP 57

END PROGRAM

