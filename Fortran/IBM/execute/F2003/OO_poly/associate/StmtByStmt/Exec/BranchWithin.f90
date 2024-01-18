! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  BranchWithin.f  
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
!*  TEST CASE NAME             : BranchWithin
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
!*  Branch within associate and do constructs 
!* ( ICE)
!*  
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM BranchWithin
  IMPLICIT NONE
 
  INTEGER :: i,j

1 D1:DO j = 1, 5
    GOTO 2
2   A1:ASSOCIATE (As0  => i )
      GOTO 3
3     D2: DO i = 1, 5
        GOTO 4
4       A2:ASSOCIATE (As1  => As0 )
5         GOTO 6 
6       END ASSOCIATE A2
        GOTO 7
7     END DO D2
8     PRINT*, "OK!"
      GOTO 9
9   END ASSOCIATE A1
    GOTO 10
10 END DO D1 

  END
