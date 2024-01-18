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

PROGRAM  FINDLOC_CHARACTER
implicit none

!-- all values are known at compile time:
integer, parameter :: kind1 = 4, d = 1
character(3), parameter :: arr(5) = ["ab1", "ab2", "ab3", "ab2","ab4"], val = "ab2"
logical, parameter :: m(5) = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.], b=.TRUE.
integer, parameter :: result_a_v(*) = findloc(arr, val)
integer, parameter :: result_a_v_d = findloc(arr, val, d)
integer, parameter :: result_a_v_d_m = findloc(arr, val, 1, m)
integer(kind1), parameter :: result_a_v_m_k_b(*) = findloc(arr, val, m, kind1, b)
integer, parameter :: result_a_v_m_b(*) = findloc(arr, val, m, BACK=b)

!-- in the following cases FE does not know the values @ compile time:
character(5) :: arr1(5), arr2(-3:1), arr2d(3,4), arr3d(2,4,3), val1
logical :: mask1(5), mask2(5), mask3(-2:2),  s_mask, back1, back2, mask2d(3,4), mask3d(2,4,3)
integer :: dim1, i, j, k, t, l

arr1 = ["ab1", "ab2", "ab3", "ab2", "ab4"]
arr2 = arr1

!-- initializing arr2d s.t. it has the value: ab1, ab2, ab3, ...., ab12
do i = 1, 3
 do j = 1,4
  arr2d(i,j) = ''
  t = (j-1)*3 + i
  do while (t > 0)
    k = mod(t,10) + ichar("0")
    arr2d(i,j)= char(k) // arr2d(i,j)
    t = t / 10
  end do
  arr2d(i,j) =  "ab" // arr2d(i,j)
 end do
end do
!print *,arr2d

do i = 1, 2
 do j = 1,4
  do l = 1,3
   arr3d(i,j,l) = ''
   t = (j-1)*2  +(l-1)*8+ i
   do while (t > 0)
     k = mod(t,10) + ichar("0")
     arr3d(i,j,l)= char(k) // arr3d(i,j,l)
     t = t / 10
   end do
  arr3d(i,j,l) =  "ab" // arr3d(i,j,l)
  end do
 end do
end do
!print *,arr3d

val1 = "ab2"

arr2d(2,2) = "ab2"
arr3d(1,2,1) = "ab2"
arr3d(1,1,2) = "ab2"
arr2d(3,3) = "ab2"
arr3d(2,3,3) = "ab2"

dim1 = 1

back1 = .TRUE.
back2 = .FALSE.

s_mask = .true.
mask1 = [.TRUE., .TRUE., .TRUE. ,.TRUE., .TRUE.]
mask2 = [.FALSE., .TRUE., .TRUE., .TRUE., .TRUE.]
mask3 = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.]
mask2d = reshape ([.TRUE., .FALSE., .FALSE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .FALSE., .TRUE., .TRUE., .TRUE.], shape(arr2d))

mask3d = reshape([.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .FALSE., .TRUE.], shape(arr3d))


!-- comparing the result when some of the arguments are known at compile time vs all of them vs none of them:
if (ANY(result_a_v /= findloc(arr1, val1))) ERROR STOP 100
if (result_a_v_d /= findloc(arr1, val1, dim1)) ERROR STOP 101
if (ANY(result_a_v_m_k_b /= findloc(arr1, val1, mask1, kind1, back1))) ERROR STOP 103
if (ANY(result_a_v_m_b /= findloc( ["ab1", "ab2", "ab3", "ab2","ab4"],&
                                    "ab2", [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.],&
                                    BACK=.TRUE.))) ERROR STOP 104

if (ANY(findloc(arr, val) /= findloc(arr1, val1))) ERROR STOP 10
!-- RTE should fix dim problem
if (findloc(arr, val, d) /= findloc(arr1, val1, dim1)) ERROR STOP 11
if (ANY(findloc(arr, val1) /= findloc(arr1, val))) ERROR STOP 12
if (ANY(findloc(arr, val, m, kind1, b) /= findloc(arr1, val1, mask1, kind1, back1))) ERROR STOP 13
!-- for now because of a problem in wcode++ we can not pass second arg directly into findloc when it is character.
if (ANY(findloc(arr, val, m, BACK=b) /= findloc( ["ab1", "ab2", "ab3", "ab2","ab4"],&
                                                  "ab2", [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.],&
                                                  BACK=.TRUE.))) ERROR STOP 14

if (ANY(findloc(arr1, val, mask1, BACK = .false.) /= [2,1])) ERROR STOP 15

if (findloc(arr1, val, 1, mask1, 4, .false.) /= 2) ERROR STOP 16

!--  when all args are present:
if (findloc(arr1, val, 1, mask1, 4, .true.) /= 4) ERROR STOP 17
!-- using the argument keywords:
if (findloc(VALUE=val, ARRAY=arr1, BACK=.false., KIND=4, MASK=mask1, DIM=1) /= 2) STOP 18

!-- with different kind arg:
if (kind(findloc(arr1, val, 1, mask1, 1, .true.)) /= 1) ERROR STOP 19
if (kind(findloc(arr1, val, 1, mask1, 2, .true.)) /= 2) ERROR STOP 20
if (kind(findloc(arr1, val, 1, mask1, BACK=.true.)) /= 4) ERROR STOP 21
if (kind(findloc(arr1, val, 1, mask1, 8, .true.)) /= 8) ERROR STOP 22


!-- when KIND argument is absent:
if (findloc(arr1, val, 1, MASK=mask1, BACK = .TRUE.) /= 4) ERROR STOP 23

!-- when DIM argument is absent,the result is an array of rank one and of size equal to the rank of arr1:
if (ANY(findloc(arr1, "ab2", mask1, 4, .false.) /= [2])) ERROR STOP 24

!-- when MASK argument is absent:
 !--for now it does not work becasue of dim1. RTE should fix it!
if (findloc(arr1, val1, dim1, KIND=kind1,BACK=.TRUE.) /= 4) ERROR STOP 25

if (findloc(arr1, val1, 1, KIND=kind1, BACK=.true.) /= 4) ERROR STOP 26

!-- if MASK argument is present and some of its elemts are false versus a mask with all true elements:
 ! should work after FE fix
if (findloc(arr1, "ab1", 1, mask1) /= 1) ERROR STOP 27
if (findloc(arr1, "ab1", 1, mask2) /= 0) ERROR STOP 28

!-- when BACK argument is absent:
if (findloc(arr1, val1, 1, mask1, kind1) /= findloc(arr1, val1, 1, mask1, kind1, .false.)) ERROR STOP 29

!-- when MASK is a scalar
if (findloc(arr1, val1, 1, mask1, kind1) /= findloc(arr1, val1, 1, s_mask, kind1)) ERROR STOP 30

!-- test with different lower bounds of ARRAY/MASK argument:
if (findloc(arr1, val1, 1, mask1) /= findloc(arr2, val1, 1, mask1)) ERROR STOP 31
if (findloc(arr1, val1, 1) /= findloc(arr2, val1, 1)) ERROR STOP 32
if (findloc(arr1, val1, 1, mask3) /= findloc(arr2, val1, 1, mask1)) ERROR STOP 33

!-- test with 2-dimensional array:
if (ANY(findloc(arr2d, val ) /= [2,1])) ERROR STOP 34
if (ANY(findloc(arr2d, val, mask2d) /= [2,2])) ERROR STOP 35
if (ANY(findloc(arr2d, val, s_mask) /= [2,1])) ERROR STOP 36

if (ANY(findloc(arr2d, val, 1) /= [2,2,3,0])) ERROR STOP 37
if (ANY(findloc(arr2d, val, 2) /= [0,1,3])) ERROR STOP 38

if (ANY(findloc(arr2d, val1, 1, mask2d) /= [0,2,0,0])) ERROR STOP 39
if (ANY(findloc(arr2d, val1, 2, mask2d) /= [0,2,0])) ERROR STOP 40

if (ANY(findloc(arr2d, val1, 1, mask2d, 2) /= [0,2,0,0])) ERROR STOP 41

if (ANY(findloc(arr2d, val1, BACK=.TRUE.) /= [3,3])) ERROR STOP 42
if (ANY(findloc(arr2d, val1, mask2d, BACK=.TRUE.) /= [2,2])) ERROR STOP 43

if (ANY(findloc(arr2d, val1, 2) /= [0,1,3])) ERROR STOP 44
if (ANY(findloc(arr2d, val1, 2, BACK=.FALSE.) /= [0,1,3])) ERROR STOP 45
if (ANY(findloc(arr2d, val1, 2, BACK=.TRUE.) /= [0,2,3])) ERROR STOP 46

!-- test with 3-dimensional array:
if (ANY(findloc(arr3d, val1) /= [2,1,1])) ERROR STOP 47

if (ANY(findloc(arr3d, val1, 1) /= reshape([2,1,0,0,1,0,0,0,0,0,2,0],[4,3]))) ERROR STOP 48
if (ANY(findloc(arr3d, val1, 2) /= reshape([2,1,1,0,0,3],[2,3]))) ERROR STOP 49
if (ANY(findloc(arr3d, val1, 3) /= reshape([2,1,1,0,0,3,0,0],[2,4]))) ERROR STOP  50

if (ANY(findloc(arr3d, val1, mask3d, BACK=.TRUE.) /= [1,1,2])) ERROR STOP 51

END PROGRAM



