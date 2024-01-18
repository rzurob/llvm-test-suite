! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-05-27
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FINDLOC intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FINDLOC (ARRAY, VALUE, DIM [, MASK, KIND, BACK]) or
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

PROGRAM FINDLOC_REAL

!-- all values are known at compile time:
integer :: arr(5) = (/1, 4, 3, 4, 5/), val = 4, d = 1
logical :: m(5) = (/.TRUE., .TRUE., .TRUE., .TRUE., .TRUE./), b=.TRUE.

!-- in the following cases FE does not know the values @ compile time:
integer  :: arr1(5), arr2(-3:1), arr2d(3,4), arr3d(2,4,3)
logical  :: mask1(5), mask2(5), mask3(-2:2),  s_mask, back1, back2, mask2d(3,4), mask3d(2,4,3)
integer :: val1, dim1, val_input
integer, parameter :: kind1=4


arr1 = (/1, 4, 3, 4, 5/)
arr2 = arr1
arr2d = reshape((/1, 2, 3, 4, 5, 3, 6, 7, 3, 8, 9, 9/), (/3,4/))
arr3d = reshape ((/1,2,3,4,5,6,3,7,8,9,10,11,12,13,14,3,15,16,17,18,19,20,3,21/), (/2,4,3/))
val1 = 4
dim1 = 1

back1 = .TRUE.
back2 = .FALSE.

s_mask = .true.
mask1 = (/.TRUE., .TRUE., .TRUE. ,.TRUE., .TRUE./)
mask2 = (/.FALSE., .TRUE., .TRUE., .TRUE., .TRUE./)
mask3 = (/.TRUE., .TRUE., .TRUE., .TRUE., .TRUE./)
mask2d = reshape ((/.TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .FALSE., .TRUE., .TRUE., .TRUE./), shape(arr2d))

mask3d = reshape((/.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE./), shape(arr3d))


!-- comparing the result when some of the arguments are known at compile time vs all of them vs none of them:
if (ANY(findloc(arr, val) .NE. findloc(arr1, val1))) ERROR STOP 10
!-- RTE should fix dim problem
if (findloc(arr, val, d) .NE. findloc(arr1, val1, dim1)) ERROR STOP 11
if (ANY(findloc(arr, val1) .NE. findloc(arr1, val))) ERROR STOP 12
if (ANY(findloc(arr, val, m, kind1, b) .NE. findloc(arr1, val1, mask1, kind1, back1))) ERROR STOP 13
if (ANY(findloc(arr, val, m, BACK=b) .NE. findloc((/1, 4, 3, 4, 5/), 4,&
                                                  (/.TRUE., .TRUE., .TRUE., .TRUE., .TRUE./),&
                                                  BACK=.TRUE.))) ERROR STOP 14

if (findloc(arr1, 4, 1, MASK=mask1, BACK = .TRUE.) .NE. 4) ERROR STOP 15

if (ANY(findloc(arr1, 4, Mask=mask1, BACK = .false.) .NE. (/2/))) ERROR STOP 16

if (findloc(arr1, 4, 1, mask1, 4, .false.) .NE. 2) ERROR STOP 17

!--  when all args are present:
if (findloc(arr1, 4, 1, mask1, 4, .true.) .NE. 4) ERROR STOP 18
!-- using the argument keywords:
if (findloc(VALUE=4, ARRAY=arr1, BACK=.false., KIND=4, MASK=mask1, DIM=1) .NE. 2) STOP 19

!-- with different kind arg:
if (kind(findloc(arr1, 4, 1, mask1, 1, .true.)) .NE. 1) ERROR STOP 20
if (kind(findloc(arr1, 4, 1, mask1, 2, .true.)) .NE. 2) ERROR STOP 21
if (kind(findloc(arr1, 4, 1, mask1, BACK=.true.)) .NE. 4) ERROR STOP 22
if (kind(findloc(arr1, 4, 1, mask1, 8, .true.)) .NE. 8) ERROR STOP 23


!-- when KIND argument is absent:
if (findloc(arr1, 4, 1, MASK=mask1, BACK = .TRUE.) .NE. 4) ERROR STOP 24

!-- when DIM argument is absent,the result is an array of rank one and of size equal to the rank of arr1:
if (ANY(findloc(arr1, 4, mask1, 4, .false.) .NE. (/2/))) ERROR STOP 25

!-- when MASK argument is absent:
 ! for now it does not work becasue of dim1. RTE should fix it!
if (findloc(arr1, val1, dim1, KIND=kind1,BACK=.TRUE.) .NE. 4) ERROR STOP 26

if (findloc(arr1, val1, 1, KIND=kind1, BACK=.true.) .NE. 4) ERROR STOP 27

!-- if MASK argument is present and some of its elemts are false versus a mask with all true elements:
if (findloc(arr1, 1, 1, mask1) .NE. 1) ERROR STOP 28
if (findloc(arr1, 1, 1, mask2) .NE. 0) ERROR STOP 29

!-- when BACK argument is absent:
if (findloc(arr1, val1, 1, mask1, kind1) .NE. findloc(arr1, val1, 1, mask1, kind1, .false.)) ERROR STOP 30

!-- when MASK is a scalar
if (findloc(arr1, val1, 1, mask1, kind1) .NE. findloc(arr1, val1, 1, s_mask, kind1)) ERROR STOP 31

!-- test with different lower bounds of ARRAY argument:
if (findloc(arr1, val1, 1, mask1) .NE. findloc(arr2, val1, 1, mask1)) ERROR STOP 32
if (findloc(arr1, val1, 1) .NE. findloc(arr2, val1, 1)) ERROR STOP 33
if (findloc(arr1, val1, 1, mask3) .NE. findloc(arr2, val1, 1, mask1)) ERROR STOP 34

!-- test with 2-dimensional array:
if (ANY(findloc(arr2d, 3) .NE. (/3,1/))) ERROR STOP 35
if (ANY(findloc(arr2d, 3, mask2d) .NE. (/3,2/))) ERROR STOP 36
if (ANY(findloc(arr2d, 3, s_mask) .NE. (/3,1/))) ERROR STOP 37

if (ANY(findloc(arr2d, 3, 1) .NE. (/3,3,3,0/))) ERROR STOP 38
if (ANY(findloc(arr2d, 3, 2) .NE. (/0,0,1/))) ERROR STOP 39

if (ANY(findloc(arr2d, 3, 1, mask2d) .NE. (/0,3,0,0/))) ERROR STOP 40
if (ANY(findloc(arr2d, 3, 2, mask2d) .NE. (/0,0,2/))) ERROR STOP 41


if (ANY(findloc(arr2d, 3, 1, mask2d, 2) .NE. (/0,3,0,0/))) ERROR STOP 42

if (ANY(findloc(arr2d, 3, BACK=.TRUE.) .NE. (/3,3/))) ERROR STOP 43
if (ANY(findloc(arr2d, 3, mask2d, BACK=.TRUE.) .NE. (/3,2/))) ERROR STOP 44

if (ANY(findloc(arr2d, 9, 1) .NE. (/0,0,0,2/))) ERROR STOP 45
if (ANY(findloc(arr2d, 9, 1, BACK=.FALSE.) .NE. (/0,0,0,2/))) ERROR STOP 46
if (ANY(findloc(arr2d, 9, 1, BACK=.TRUE.) .NE. (/0,0,0,3/))) ERROR STOP 47

!-- test with 3-dimensional array:
if (ANY(findloc(arr3d, 3) .NE. (/1,2,1/))) ERROR STOP 48

if (ANY(findloc(arr3d, 3, 1) .NE. reshape((/0,1,0,1,0,0,0,2,0,0,0,1/),(/4,3/)))) ERROR STOP 49
if (ANY(findloc(arr3d, 3, 2) .NE. reshape((/2,0,0,4,4,0/),(/2,3/)))) ERROR STOP 50
if (ANY(findloc(arr3d, 3, 3) .NE. reshape((/0,0,1,0,0,0,1,2/),(/2,4/)))) ERROR STOP  51

if (ANY(findloc(arr3d, 3, mask3d, BACK=.TRUE.) .NE. (/2,4,2/))) ERROR STOP 52


END PROGRAM



