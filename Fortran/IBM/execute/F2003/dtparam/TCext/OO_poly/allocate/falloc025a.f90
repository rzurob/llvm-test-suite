! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc025a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc025a.f
! %VERIFY: falloc025a.out:falloc025a.vf
! %STDIN:
! %STDOUT: falloc025a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (when a variable is deallocated, any
!                               allocated allocatable subobject of the variable
!                               is deallocated)
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

        final :: finalizeBase
    end type

    type A(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data

        contains

        final :: finalizeA
    end type

    type B(k3)    ! (4)
        integer, kind            :: k3
        type(A(k3)), allocatable :: data
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: B

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeA (a1)
        type (A(4)), intent(in) :: a1

        print *, 'finalizeA'
    end subroutine
end module

program falloc025a
use m
    type (B(4)), pointer :: b1

    allocate (b1)
    allocate (b1%data)
    allocate (b1%data%data)

    print *, 'begin'

    deallocate (b1)

    print *, 'end'
end
