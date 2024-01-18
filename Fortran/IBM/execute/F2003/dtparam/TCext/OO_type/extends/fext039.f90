!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext039.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (unlimited poly-allocatable component
!                               in derrived type components)
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

program fext039
    type base(k1)
        integer, kind :: k1
        class(*), allocatable :: data1(:)
    end type

    type, extends(base) :: child(k2,n)
        integer, kind :: k2
        integer, len :: n = 20
        class(*), allocatable :: data2
    end type

    class (base(4)), allocatable :: b1, b2(:)

    allocate (b1, b2(-1:0))

    if (allocated (b1%data1)) error stop 1_4

    if (allocated (b2(-1)%data1) .or. allocated (b2(0)%data1)) error stop 2_4

    deallocate (b2)

    allocate (b2(1), source = child(4,4,20)((/1,2/), data2 = 1.0e0))
end
