!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext100.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived type (an empty type; allocatable and
!*                               pointer array in allocate statement)
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

program fext100
    type base(k1,n)
        integer, kind :: k1
        integer, len :: n=20
    end type

    class (base(4,20)), allocatable :: b1(:)
    class (base(4)), pointer :: b2(:)

    allocate (b1(10), source=base(4,20)())
    allocate (b2(20), source=base(4,20)())

    deallocate (b2, b1)
end
