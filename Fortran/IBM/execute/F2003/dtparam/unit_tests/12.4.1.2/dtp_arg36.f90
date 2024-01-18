!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSTIC TESTED          : Function return is a f90 pointer to a derived
!*                               type object with non-constant type parameters
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type Base(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d)
  integer :: avar
end type

type(Base(4, 2)), pointer :: rest

rest => func1(2)

! verify the function result
if (rest%avar /= 100) stop 1
contains

function func1(nn)
integer nn
! function result is a f90 pointer and defined as 'Base' type with
! non-constant type parameter
type(Base(4, nn)), pointer :: func1

allocate(func1)
func1%avar = 100
end function

end
