!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn016a_1.f
! %VERIFY: fpAssgn016a_1.out:fpAssgn016a_1.vf
! %STDIN:
! %STDOUT: fpAssgn016a_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (in forall construct;
!*                               intrinsic assignment will do pointer assignment
!*                               for pointer structure component)
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
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    type (base), target :: b1_m(10)
    type (child), target :: c1_m(10)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container
        class (base), pointer :: data => null()
    end type
end module

program fpAssgn016a_1
use m1
    type (container) :: co1(10)

    b1_m = (/(base(i), i=1,10)/)

    c1_m = (/(child(i+1, 'c1_m_'//char(ichar ('0')+i)), i=0,9)/)

    forall (i=1:10, (mod (i,2) == 0))
        co1(i) = container (data = b1_m(i))
    end forall

    forall (i=1:10, (mod (i,2) /= 0))
        co1(i) = container (data = c1_m(i))
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
    end do

end
