!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : pass pointer argument with deferred
!*                               type parameters. Also dummy's type
!*                               parameter list contains an unknown
!*                               expression value.
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

call sub1(aptr, 4)

contains
subroutine sub1(pa, len_tp)
integer len_tp
type(humongous_matrix(4, len_tp, :)), pointer :: pa
type(humongous_matrix(4, 4, 2)), pointer :: tgt

! - check if dummy inherits actual argument's type parameter
! - values as well as type parameter expression values
if (any(ubound(pa%element) .ne. (/4, 4/))) stop 1 ! - failed 'cause TPV table
                                                  ! - of actual and dummy does
                                                  ! - not match.

allocate(tgt)

tgt%element = 8

pa => tgt
pa%avar = pa%element(1, 2) * 2

! - check if pointer gets updated
if (any(ubound(pa%element) .ne. (/4, 2/))) stop 2
if (any(pa%element .ne. 8)) stop 4
if (pa%avar .ne. 16) stop 5

end subroutine

end
