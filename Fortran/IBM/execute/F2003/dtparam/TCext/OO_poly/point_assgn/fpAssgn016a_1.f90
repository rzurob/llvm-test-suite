! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn016a_1.f
! opt variations: -qck -qnol -qdefaultpv -qdeferredlp -qreuse=none

! SCCS ID Information
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (20,4)
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    type (base(20,4)), target :: b1_m(10)
    type (child(20,4)), target :: c1_m(10)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container(n2,k2)    ! (20,4)
        integer, kind               :: k2
        integer, len                :: n2
        class(base(n2,k2)), pointer :: data => null()
    end type
end module

program fpAssgn016a_1
use m1
    type (container(20,4)) :: co1(10)

    b1_m = (/(base(20,4)(i), i=1,10)/)

    c1_m = (/(child(20,4)(i+1, 'c1_m_'//char(ichar ('0')+i)), i=0,9)/)

    forall (i=1:10, (mod (i,2) == 0))
        co1(i) = container(20,4) (data = b1_m(i))
    end forall

    forall (i=1:10, (mod (i,2) /= 0))
        co1(i) = container(20,4) (data = c1_m(i))
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
    end do

end
