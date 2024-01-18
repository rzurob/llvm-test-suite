! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/procInterface002a.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnok -ql

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated procedure is either an
!                              external function or an external
!                              subroutine. Non-poly. Intrinsic or
!                              derived type, scalar.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(), nopass, pointer :: pp1
        procedure(sub2), nopass, pointer :: pp2
    end type

    interface
        subroutine sub2(b)
            import base
            type(Base(4)), intent(in) :: b(10)
        end subroutine
    end interface
end module

program procInterface002a
use m
    interface
        subroutine sub1(i)
            integer, intent(in) :: i(2,3)
        end subroutine
    end interface

    type(Container(4)) :: c1

    c1%pp1 => sub1
    call c1%pp1(reshape((/(i,i=1,6)/),(/2,3/)))

    c1%pp2 => sub2
    call c1%pp2((/(Base(4)(i),i=1,10)/))
end

subroutine sub1(i)
    integer, intent(in) :: i(2,3)
    print *, "sub1", i
end subroutine

subroutine sub2(b)
use m
    type(Base(4)), intent(in) :: b(10)
    print *, "sub2", b
end subroutine
