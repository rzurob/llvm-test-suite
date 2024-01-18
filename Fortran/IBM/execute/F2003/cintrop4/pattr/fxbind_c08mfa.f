! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     : Interoperable procedure contained
!*                                in module.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - main written in C, C  calls FORTRAN procedure.
!*   - The interoperable  procedure itself is  implemented using Fortran
!*     procedure.
!*   - Test BIND(C) attribute with I/O in External Fortran Procedure.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module check
contains

! A routine to read in the data from a file and change the
! value of array argument.

SUBROUTINE Read_Data(Raw_Data,How_Many) BIND(C)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many
INTEGER , INTENT(INOUT) , DIMENSION(1:How_Many) :: Raw_Data

INTEGER :: I

  OPEN(FILE='fxbind_c08mfa.dat',UNIT=1)
  DO I=1,How_Many
    READ (UNIT=1,FMT=*) Raw_Data(I)
    Raw_Data(I) = i + 1
  ENDDO

END SUBROUTINE Read_Data

end module check
