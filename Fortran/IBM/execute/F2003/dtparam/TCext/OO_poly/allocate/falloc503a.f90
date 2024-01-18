! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc503a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc503a.f
! %VERIFY: falloc503a.out:falloc503a.vf
! %STDIN:
! %STDOUT: falloc503a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (allocate and deallocate the
!                               allocatable subobject with final binding)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        final :: finalizeChild
    end type

    type container(k3)    ! (4)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,1,*)), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program falloc503a
use m
    type (container(4)) :: co1

    allocate (child(4,1,20) :: co1%data)

    deallocate (co1%data)
end
