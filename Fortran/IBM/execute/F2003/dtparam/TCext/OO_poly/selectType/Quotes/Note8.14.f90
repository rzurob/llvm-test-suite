! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Note8.14.f
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
!*  TEST CASE NAME             : Note8.14 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 27, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*  Quoted from Note 8.14 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      TYPE POINT
        REAL :: X=1.0, Y=1.0
      END TYPE POINT

      TYPE, EXTENDS(POINT) :: POINT_3D
        REAL :: Z=1.0
      END TYPE POINT_3D

      TYPE, EXTENDS(POINT) :: COLOR_POINT
        INTEGER :: COLOR
      END TYPE COLOR_POINT

      TYPE(POINT), TARGET :: P
      TYPE(POINT_3D), TARGET :: P3
      TYPE(COLOR_POINT), TARGET :: C
      CLASS(POINT), POINTER :: P_OR_C

      P_OR_C => P3
      SELECT TYPE ( P_OR_C )
      CLASS IS ( POINT )
        ! "CLASS ( POINT ) :: P_OR_C" implied here
        PRINT *, P_OR_C%X, P_OR_C%Y
      TYPE IS ( POINT_3D )
        ! "TYPE ( POINT_3D ) :: P_OR_C" implied here
        PRINT *, P_OR_C%X, P_OR_C%Y, P_OR_C%Z ! This block gets executed
        IF (P_OR_C%X .NE. 1.0 .OR. P_OR_C%Y .NE. 1.0 .OR. P_OR_C%Z .NE. 1.0 ) STOP 11
      END SELECT


      END


