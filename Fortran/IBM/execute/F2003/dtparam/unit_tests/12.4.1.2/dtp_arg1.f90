!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pointer assignment with DTP
!*                             : 
!*  PROGRAMMER                 : Huiwen Li
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONAL TESTED          : Verify the type parameter values and contents
!*                               of type components
!*                                -- check length type parameter
!*                                -- check the array bounds
!*                                -- check the array element and section
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

 type humongous_matrix(k, d)
   integer, kind :: k
   integer, len  :: d
   integer :: element(d, d/2)
   integer :: avar
 end type

type(humongous_matrix(4, :)), pointer :: aptr
type(humongous_matrix(4, 4)), target  :: tgtt
integer rest1, rest2(4, 2), rp(4, 2), rp1(2, 4)

tgtt%element = 22
tgtt%avar = 102

! - pointer assignment sets type parameter info
aptr => tgtt

! - scalar and array assignments
aptr%element(:, 1) = 11
rest1 = aptr%element(1, 2)
rest2 = aptr%element

! - verify type parameter information
if (aptr%d .ne. 4) stop 1
if (any(ubound(aptr%element) .ne. (/4, 2/))) stop 2


! - verify the component values
if (rest1 .ne. 22) stop 4

rp = reshape((/11, 11, 11, 11, 22, 22, 22, 22/), (/4, 2/))
if (any(aptr%element .ne. rp)) stop 5

rp1 = reshape((/11, 11, 11, 11, 22, 22, 22, 22/), (/2, 4/))
if (any(reshape(aptr%element, (/2, 4/)) .ne. rp1)) stop 6

if (aptr%avar .ne. 102) stop 7
end
