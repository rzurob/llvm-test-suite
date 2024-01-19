! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/typCompatible/sameDT4both1.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of an nonpoly DT
!*                               move_alloc appears in module proc
!*                               TO is dummy arg/module var
!*                               FROM is private module var
!*                               defect 322404
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(k1)    ! (4)
        integer, kind :: k1
        character(kind=1, len=:), allocatable :: ch(:)
    end type

    type(A(4)), allocatable :: a1, a2
    private :: a1

    contains
        subroutine sub()
            allocate(a1, source = A(4)( (/ (repeat('xyz ',i), i=1,1)  /)) )

            call move_alloc(a1, a2)
        end subroutine

        subroutine sub1(arg)
            type(A(4)), allocatable :: arg

            allocate(a1, source = A(4) ( (/ (repeat('xyz ',i), i= 0,0)  /)) )
            call move_ALLOC( a1, arg)

        end subroutine

end module

use m
    call sub()
    print *, a2%ch

    call sub1(a2)
    print *, size(a2%ch,1), len(a2%ch(1))
   end
