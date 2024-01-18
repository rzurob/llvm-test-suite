! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc010a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc010a1.f
! %VERIFY: falloc010a1.out:falloc010a1.vf
! %STDIN:
! %STDOUT: falloc010a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (variables with unallocated
!                               allocatable component in source-expr)
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
    type base(k1,n1)    ! (1,17)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n2)    ! (1,17,4,20)
        integer, kind :: k2
        integer, len  :: n2
        class (*), allocatable :: data(:)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,4,*)), intent(in) :: b

        if (allocated (b%data)) then
            print *, b%name, 'size of data :', size(b%data)
        else
            print *, b%name, 'data is not allocated'
        end if
    end subroutine
end module

program falloc010a1
use m
    type (child(1,17,4,20)) :: c1 = child(1,17,4,20)('xlftest', null())
    class (base(1,17)), pointer :: b1
    class (child(1,17,4,20)), allocatable :: c2

    allocate(b1, source=c1)
    allocate (c2, source=c1)

    call b1%print
    call c2%print
end
