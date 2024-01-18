! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : sameDT4both.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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
    type A
        character(kind=1, len=:), allocatable :: ch(:)
    end type 

    type(A), allocatable :: a1, a2
    private :: a1

    contains
        subroutine sub()
            allocate(a1, source = A( (/ (repeat('xyz ',i), i=1,1)  /)) )

            call move_alloc(a1, a2)
        end subroutine

        subroutine sub1(arg)
            type(A), allocatable :: arg 

            allocate(a1, source = A ( (/ (repeat('xyz ',i), i= 0,0)  /)) )
            call move_ALLOC( a1, arg)

        end subroutine

end module

use m
    call sub()
    print *, a2%ch 

    call sub1(a2)
    print *, size(a2%ch,1), len(a2%ch(1))
   end
