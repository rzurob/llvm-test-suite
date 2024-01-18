! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  HostAssocConstStr.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocConstStr 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is an associte name associating to a constant string 
!*   (Core dump)   
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM HostAssocConstStr
  IMPLICIT NONE

  INTEGER                  :: i
  CHARACTER(10), PARAMETER :: Str1="1234567890"
  CHARACTER(1), PARAMETER, DIMENSION(10) :: StrArr=(/ (ACHAR(IACHAR("0") + i), i=0, 9)/)
 
    ASSOCIATE ( T0 => Str1(1:9), T1=>StrArr(1:10:2) )
    ASSOCIATE ( As0 => T0, As1 => T1)
      IF ( As0 .NE. "123456789" ) STOP 40
      IF ( .NOT. ALL (As1 .EQ. (/ (ACHAR(IACHAR("0") + i), i=0, 9, 2)/)) ) STOP 41 
    END ASSOCIATE
    END ASSOCIATE

  END
