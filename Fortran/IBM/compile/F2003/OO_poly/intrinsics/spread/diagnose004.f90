! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp diagnose004.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/04/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    DIM shall be scalar and of type integer with value in the range
!*  of 1<=DIM<=n+1, where n is th rank of SOURCE.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY : 03/23/74
!*                        Init : yongdu@ca.ibm.com
!*                    Comments : 1) Removed TRUN header.
!*                               2) The old version tried to print out
!*                               the return value of spread, which is
!*                               not allowed for regualr IO. Modified
!*                               to use associate construct.
!*                               3) The verification file is updated.
!*                               The current driver output the error
!*                               message twice. If in the future the
!*                               driver is modified to output the error
!*                               message just once, then the
!*                               verification file needs to be updated
!*                               again.
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer :: i = -1
    end type
end module

program diagnose004
use m
    type(Base) :: b1(2,2)
    associate(name1=>spread(b1, 1.0, 2))
    end associate
end
