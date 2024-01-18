!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : pass pointer argument with deferred
!*                               type parameters
!*                                -- check type parameter values
!*                                -- check the component values
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type humongous_matrix(k, d, m)
  integer, kind :: k
  integer, len  :: d
  integer, len  :: m
  integer :: element(d, m)
  integer :: avar
end type

type(humongous_matrix(4, 4, :)), pointer :: aptr
type(humongous_matrix(4, 4, 4)), target  :: ttt

ttt%element =12
aptr => ttt

call sub1(aptr)

if (aptr%m .ne. 2) stop 6
if (any(ubound(aptr%element) .ne. (/4, 2/))) stop 7
if (aptr%avar .ne. 16) stop 8

contains
subroutine sub1(pa)
type(humongous_matrix(4, 4, :)), pointer :: pa
type(humongous_matrix(4, 4, 2)), pointer :: tgt

if(any(ubound(pa%element) .ne. (/4, 4/))) stop 1
if(any(pa%element .ne. 12)) stop 2

allocate(tgt)

tgt%element = 8
pa => tgt
pa%avar = pa%element(1, 2) * 2
if (any(ubound(pa%element) .ne. (/4, 2/))) stop 4
if (any(pa%element .ne. 8)) stop 5

end subroutine

end
