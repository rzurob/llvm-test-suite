! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/spread/diagnose004.f
! opt variations: -qnol

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/04/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = -1
    end type
end module

program diagnose004
use m
    type(Base(20,4)) :: b1(2,2)
    associate(name1=>spread(b1, 1.0, 2))
    end associate
end
