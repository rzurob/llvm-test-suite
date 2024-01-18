! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/component1/sequenceType004.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnol -qreuse=self

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer component for
!                              sequence type. Do not specify
!                              proc-interface. Associate the procedure
!                              pointer component with either an external
!                              function or an external subroutine.
!
!                              Use default implicit typing to match
!                              the procedure pointer with the target.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1,k2)    ! (20,4,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i
        procedure(), nopass, pointer :: ip
        integer(k2)      j
        procedure(sub2), nopass, pointer :: rp
    end type

    interface
        subroutine sub2(b)
            import base
            type(Base(*,4,4)), intent(in) :: b
        end subroutine
    end interface

end module

program sequenceType004
use m
    interface
        subroutine sub1(i)
            integer, intent(in) :: i(2,3)
        end subroutine
    end interface

    type(Base(20,4,4)) :: b1
    b1%ip => sub1
    b1%rp => sub2

    call b1%ip((/(i,i=1,6)/))
    call b1%rp(Base(20,4,4)(5,null(),10,null()))
end

subroutine sub1(i)
    integer, intent(in) :: i(2,3)
    print *, "sub1", i
end subroutine

subroutine sub2(b)
use m
    type(Base(*,4,4)), intent(in) :: b
    print *, "sub2", b%i, b%j
end subroutine
