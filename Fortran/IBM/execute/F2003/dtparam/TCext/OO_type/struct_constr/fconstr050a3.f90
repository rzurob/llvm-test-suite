! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr050a3.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr050a3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (array section used as
!                               the data-source for nonpoly-allocatable
!                               component)
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

    type, extends(base) :: child(k2,n1)    ! (4,1,10)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program fconstr050a3
use m
    type container(k3)    ! (4)
        integer, kind               :: k3
        type(base(k3)), allocatable :: data(:)
    end type

    class (base(4)), allocatable :: b1(:,:)

    allocate (b1(0:1,-1:0), source=reshape ((/(child(4,1,10)(i,'test_101'),i=1,4)/), &
                            (/2,2/)))


    call associate (container(4) (b1(:,0)))

    contains

!    associate (x => container(4) (b1(:,0)))
    subroutine associate (x)
        type(container(4)), intent(in) :: x
        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 2)) error stop 2_4

        if (any (x%data%id /= (/3,4/))) then
            print *,x%data%id
            error stop 3_4
        end if
    end subroutine
end
