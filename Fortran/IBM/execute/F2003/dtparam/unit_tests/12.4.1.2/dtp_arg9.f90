!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ALLOCATE statement
!*                             :
!*  PROGRAMMER                 : Huiwen Li
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONAL TESTED          : The allocation object has deferred type
!*                               parameters.
!*                               - check the component bounds
!*                               - check the component values
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

type base_type(k, d)
  integer*1, kind :: k
  integer*2, len  :: d
  integer :: element(d)
  integer :: avar
end type

class(base_type(4, :)), pointer :: aptr
class(base_type(4, :)), allocatable :: alvar
type(base_type(4, 2)), target  :: tgt

tgt%avar = 12
tgt%element = tgt%avar - 20

aptr => tgt

allocate(alvar, source=aptr)

if (alvar%k /= aptr%k) error stop 1
if (alvar%d /= aptr%d) error stop 2

if (ubound(alvar%element, 1) .ne. 2) error stop 4 ! - TPV is not updated (321640)
if (any(alvar%element .ne. aptr%element)) error stop 5
if (alvar%avar /= aptr%avar) error stop 6

end

