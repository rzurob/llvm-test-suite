PROGRAM  FINDLOC_LOGICAL
implicit none

!-- all values are known at compile time:
integer, parameter :: kind1 = 4, d = 1
logical, parameter :: arr(5) = [.TRUE., .FALSE., .TRUE., .FALSE., .TRUE.], val = .FALSE.
logical, parameter :: m(5) = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.], b=.TRUE.

integer, parameter :: result_a_v(*) = findloc(arr, val)
integer, parameter :: result_a_v_d = findloc(arr, val, d)
integer(kind1), parameter :: result_a_v_m_k_b(*) = findloc(arr, val, m, kind1, b)
integer, parameter :: result_a_v_m_b(*) = findloc(arr, val, m, BACK=b)

!-- in the following cases FE does not know the values @ compile time:
logical  :: arr1(5), arr2(-3:1), arr2d(3,4), arr3d(2,4,3), val1
logical  :: mask1(5), mask2(5), mask3(-2:2),  s_mask, back1, mask2d(3,4), mask3d(2,4,3)
integer ::  dim1

arr1 = [.TRUE., .FALSE., .TRUE., .FALSE., .TRUE.]
arr2 = arr1
arr2d = reshape([ .TRUE., .FALSE., .FALSE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.], [3,4])
arr3d = reshape ([.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,&
                  .TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.], [2,4,3])
val1 = .FALSE.
dim1 = 1

back1 = .TRUE.

s_mask = .true.
mask1 = [.TRUE., .TRUE., .TRUE. ,.TRUE., .TRUE.]
mask2 = [.TRUE., .FALSE., .TRUE., .TRUE., .TRUE.]
mask3 = [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.]
mask2d = reshape ([.TRUE., .FALSE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .FALSE., .TRUE., .TRUE., .TRUE.], shape(arr2d))

mask3d = reshape([.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  &
                  .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .TRUE.], shape(arr3d))

!-- comparing the result when some of the arguments are known at compile time vs all of them vs none of them:
if (ANY(result_a_v /= findloc(arr1, val1))) ERROR STOP 100
if (result_a_v_d /= findloc(arr1, val1, dim1)) ERROR STOP 101
if (ANY(result_a_v_m_k_b /= findloc(arr1, val1, mask1, kind1, back1))) ERROR STOP 103
if (ANY(result_a_v_m_b /= findloc([.TRUE., .FALSE., .TRUE., .FALSE., .TRUE.],&
                                  .FALSE., [.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.],&
                                  BACK=.TRUE.))) ERROR STOP 104

if (ANY(findloc(arr, val) /= result_a_v)) ERROR STOP 10
if (findloc(arr, val, d) /= result_a_v_d) ERROR STOP 11
if (ANY(findloc(arr, val1) /= findloc(arr1, val))) ERROR STOP 12
if (ANY(findloc(arr, val, m, kind1, b) /= result_a_v_m_k_b)) ERROR STOP 13
if (ANY(findloc(arr, val, m, BACK=b) /= result_a_v_m_b)) ERROR STOP 14

if (ANY(findloc(arr1, .FALSE., Mask=mask1, BACK = .false.) /= [2])) ERROR STOP 15

if (findloc(arr1, .FALSE., 1, mask1, 4, .false.) /= 2) ERROR STOP 16

!--  when all args are present:
if (findloc(arr1, .FALSE., 1, mask1, 4, .true.) /= 4) ERROR STOP 17
!-- using the argument keywords:
if (findloc(VALUE= .FALSE., ARRAY=arr1, BACK=.false., KIND=4, MASK=mask1, DIM=1) /= 2) ERROR STOP 18

!-- with different kind arg:
if (kind(findloc(arr1, .FALSE., 1, mask1, 1, .true.)) /= 1) ERROR STOP 19
if (kind(findloc(arr1, .FALSE., 1, mask1, 2, .true.)) /= 2) ERROR STOP 20
if (kind(findloc(arr1, .FALSE., 1, mask1, BACK=.true.)) /= 4) ERROR STOP 21
if (kind(findloc(arr1, .FALSE., 1, mask1, 8, .true.)) /= 8) ERROR STOP 22

!-- when KIND argument is absent:
if (findloc(arr1, .FALSE., 1, MASK=mask1, BACK = .TRUE.) /= 4) ERROR STOP 23

!-- when DIM argument is absent,the result is an array of rank one and of size equal to the rank of arr1:
if (ANY(findloc(arr1, .FALSE., mask1, 4, .false.) /= [2])) ERROR STOP 24

!-- when MASK argument is absent:
 ! for now it does not work becasue of dim1. RTE should fix it!
if (findloc(arr1, val1, dim1, KIND=kind1,BACK=.TRUE.) /= 4) ERROR STOP 25

if (findloc(arr1, val1, 1, KIND=kind1, BACK=.true.) /= 4) ERROR STOP 26

!-- if MASK argument is present and some of its elemts are false versus a mask with all true elements:
if (findloc(arr1, .FALSE., 1, mask1) /= 2) ERROR STOP 27
if (findloc(arr1, .FALSE., 1, mask2) /= 4) ERROR STOP 28

!-- when BACK argument is absent:
if (findloc(arr1, val1, 1, mask1, kind1) /= findloc(arr1, val1, 1, mask1, kind1, .false.)) ERROR STOP 29

!-- when MASK is a scalar
if (findloc(arr1, val1, 1, mask1, kind1) /= findloc(arr1, val1, 1, s_mask, kind1)) ERROR STOP 30

!-- test with different lower bounds of ARRAY/MASK argument:
if (findloc(arr1, val1, 1, mask1) /= findloc(arr2, val1, 1, mask1)) ERROR STOP 31
if (findloc(arr1, val1, 1) /= findloc(arr2, val1, 1)) ERROR STOP 32
if (findloc(arr1, val1, 1, mask3) /= findloc(arr2, val1, 1, mask1)) ERROR STOP 33

!-- test with 2-dimensional array:
if (ANY(findloc(arr2d, .FALSE.) /= [2,1])) ERROR STOP 34
if (ANY(findloc(arr2d, .FALSE., mask2d) /= [3,1])) ERROR STOP 35
if (ANY(findloc(arr2d, .FALSE., s_mask) /= [2,1])) ERROR STOP 36

if (ANY(findloc(arr2d, .FALSE., 1) /= [2,0,0,0])) ERROR STOP 37
if (ANY(findloc(arr2d, .FALSE., 2) /= [0,1,1])) ERROR STOP 38

if (ANY(findloc(arr2d, .FALSE., 1, mask2d) /= [3,0,0,0])) ERROR STOP 39
if (ANY(findloc(arr2d, .FALSE., 2, mask2d) /= [0,0,1])) ERROR STOP 40

if (ANY(findloc(arr2d, .FALSE., 1, mask2d, 2) /= [3,0,0,0])) ERROR STOP 41

if (ANY(findloc(arr2d, .FALSE., mask2d, BACK=.TRUE.) /= [3,1])) ERROR STOP 42

if (ANY(findloc(arr2d, .FALSE., 1) /= [2,0,0,0])) ERROR STOP 43
if (ANY(findloc(arr2d, .FALSE., 1, BACK=.FALSE.) /= [2,0,0,0])) ERROR STOP 44
if (ANY(findloc(arr2d, .FALSE., 1, BACK=.TRUE.) /= [3,0,0,0])) ERROR STOP 45

!-- test with 3-dimensional array:
if (ANY(findloc(arr3d, .FALSE.) /= [1,2,1])) ERROR STOP 46

if (ANY(findloc(arr3d, .FALSE., 1) /= reshape([0,1,0,0,0,0,0,0,0,0,0,0],[4,3]))) ERROR STOP 47
if (ANY(findloc(arr3d, .FALSE., 2) /= reshape([2,0,0,0,0,0],[2,3]))) ERROR STOP 48
if (ANY(findloc(arr3d, .FALSE., 3) /= reshape([0,0,1,0,0,0,0,0],[2,4]))) ERROR STOP  49

if (ANY(findloc(arr3d, .FALSE., mask3d, BACK=.TRUE.) /= [1,2,1])) ERROR STOP 50

END PROGRAM
