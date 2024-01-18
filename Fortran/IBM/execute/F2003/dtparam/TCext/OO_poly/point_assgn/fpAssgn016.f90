! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn016.f
! opt variations: -ql -qdefaultpv

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn016.f
! %VERIFY: fpAssgn016.out:fpAssgn016.vf
! %STDIN:
! %STDOUT: fpAssgn016.out
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
!*  DESCRIPTION                : data pointer assignment (pointer assignment in
!*                               forall construct)
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
        private
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    contains

    pure function makeData (i)
        type (base(4)), pointer :: makeData
        integer*4, intent(in) :: i

        allocate (makeData)

        makeData = base(4)(i)
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type container(k2)    ! (4)
        integer, kind            :: k2
        class(base(k2)), pointer :: data => null()
    end type
end module

program fpAssgn016
use m1
    type (container(4)) :: co1(10)

    forall (i=1:10, .not. associated (co1(i)%data))
        co1(i)%data => makeData (i)
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
        deallocate (co1(i)%data)
    end do

end
