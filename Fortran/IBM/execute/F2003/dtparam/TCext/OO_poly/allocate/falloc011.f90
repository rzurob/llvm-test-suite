! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc011.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc011.f
! %VERIFY: falloc011.out:falloc011.vf
! %STDIN:
! %STDOUT: falloc011.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (stat= with type-spec)
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
    type base(k1,n1)    ! (1,15)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name = 'default'

        contains

        procedure, pass(b) :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (1,15,8)
        integer, kind :: k2
        integer(k2)   :: id = -1_8

        contains

        procedure, pass(b) :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,8)), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module

program falloc011
use m
    class (base(1,15)), allocatable :: b1(:)
    class (*), pointer :: x1, x2(:), x3(:,:)

    complex(8), allocatable :: cx(:)

    integer stat

    allocate (child(1,15,8) :: b1 (100:102), x1, x2(20), stat=stat)

    if (stat /= 0) error stop 1_4

    call b1(101)%print

    allocate (complex(8) :: cx(1), x3(1,2:3), stat=stat)

    if (stat /= 0) error stop 2_4
end
