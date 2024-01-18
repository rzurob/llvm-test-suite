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
!*  FUNCTIONAL TESTED          : allocate non-poly object with SOURCE=
!*                               expression.
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!*
!*
!*  MM/DD/YY:  Init:  Comments: Defect 323310
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer :: data(n) = 0
    end type

    type, extends(base) :: child
        integer id
    end type

end module

program dtParamAlloc01
use m
    type(base(4, :)), allocatable :: b2
    type(child(4, 2)), target :: tgt
    class(base(4, :)), pointer :: aptr

    tgt%data(2) = 20
    tgt%id = 1000

    aptr => tgt

    allocate(b2, source=aptr)

    if (b2%k /= 4) stop 1
    if (b2%n /= 2) stop 2

    if( any(b2%data .ne. (/0, 20/)) ) stop 4

    ! b2 should have only contain 'base' component
    if (sizeof(b2) /=8 ) stop 5
end
