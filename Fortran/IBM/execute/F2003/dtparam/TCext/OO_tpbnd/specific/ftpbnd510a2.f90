! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/OO_tpbnd/specific/ftpbnd510a2.f
! opt variations: -qnock -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd510a2.f
! %VERIFY: ftpbnd510a2.out:ftpbnd510a2.vf
! %STDIN:
! %STDOUT: ftpbnd510a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (binding calls on named
!                               constants)
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
        integer(k1)   :: id

        contains

        procedure :: print => printBase
        procedure, nopass :: print1 => printBaseArray
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBaseArray (b)
        class (base(4)), intent(in) :: b (:)

        do i = 1, size (b)
            call b(i)%print
        end do
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program ftpbnd510a2
use m
    type (base(4)), parameter :: b_const (3) = (/base(4)(1), base(4)(2), base(4)(3)/)

    type (child(4,1,20)), parameter :: c_const (2) = (/child(4,1,20)(10, 'c1'), child(4,1,20)(20, 'c2')/)

    call b_const%print1(c_const)

    call c_const%print1 (b_const)

    do i = 3, 1, -1
        call b_const(i)%print
    end do

    call c_const(1)%print
    call c_const(2)%print()
end
