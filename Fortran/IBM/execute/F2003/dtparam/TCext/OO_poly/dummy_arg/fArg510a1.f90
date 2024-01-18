! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg510a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg510a1.f
! %VERIFY: fArg510a1.out:fArg510a1.vf
! %STDIN:
! %STDOUT: fArg510a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : dummy-arg (defined assignment using
!*                               poly-dummy-arg; interface defined in module)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    private base2Base, child2Child

    interface assignment (=)
        subroutine base2Base (b1, b2)
        import base
            class (base(4)), intent(out) :: b1
            type (base(4)), intent(in) :: b2
        end subroutine

        subroutine child2Child (c1, c2)
        import child
            class (child(4,1,*)), intent(out) :: c1
            type (child(4,1,*)), intent(in) :: c2
        end subroutine
    end interface
end module

subroutine base2Base (b1, b2)
use m
    class (base(4)), intent(out) :: b1
    type (base(4)), intent(in) :: b2

    print *, 'defined assignment for base'

    b1%id = b2%id
end subroutine

subroutine child2Child (c1, c2)
use m
    class (child(4,1,*)), intent(out) :: c1
    type (child(4,1,*)), intent(in) :: c2

    c1%base = c2%base
    c1%name = c2%name
end subroutine


program fArg510a1
use m
    type (child(4,1,20)) :: c1, c2

    class (base(4)), allocatable :: b_allo

    c1 = child(4,1,20) (100, 'c1, maybe c2')

    c2 = c1

    if ((c2%id /= 100) .or. (c2%name /= 'c1, maybe c2')) error stop 1_4

    allocate (b_allo)

    b_allo = c1%base

    if (b_allo%id /= 100) error stop 2_4
end
