! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn016a1.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=base

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn016a1.f
! %VERIFY: fpAssgn016a1.out:fpAssgn016a1.vf
! %STDIN:
! %STDOUT: fpAssgn016a1.out
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
!*  DESCRIPTION                : data pointer assignment (data pointer
!*                               assignment in the forall construct;
!*                               poly-pointer to poly-target)
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

    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        pure function makeData (id, name)
        import base, child
            class (base(20,4)), pointer :: makeData
            integer*4, intent(in) :: id
            character(*), intent(in), optional :: name
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

pure function makeData (id, name)
use m, only : base, child
    class (base(20,4)), pointer :: makeData
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name


    if (present (name)) then
        allocate (makeData, source=child(20,4,4,20)(id,name))
    else
        allocate (makeData, source=base(20,4)(id))
    end if
end function

module m1
use m
    type container(n3,k3)    ! (20,4)
        integer, kind               :: k3
        integer, len                :: n3
        class(base(n3,k3)), pointer :: data => null()
    end type

    type (container(20,4)), save :: co1(10)
end module

program fpAssgn016a1
use m1


    forall (i=1:10, (mod (i,2) == 0))
        co1(i)%data => makeData (i, 'child_'//char(ichar ('0')+i-1))
    end forall

    forall (i=1:10, (mod (i,2) /= 0))
        co1(i)%data => makeData (i)
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
        deallocate (co1(i)%data)
    end do
end
