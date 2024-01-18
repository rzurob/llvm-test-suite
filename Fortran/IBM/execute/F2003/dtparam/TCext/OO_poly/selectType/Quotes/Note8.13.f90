! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Quoted from Note 8.13
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

      P_OR_C => C
      SELECT TYPE ( A => P_OR_C )
      CLASS IS ( POINT )
        ! "CLASS ( POINT ) :: A" implied here
        PRINT *, A%X, A%Y ! This block gets executed
        IF (A%X .NE. 1.0 .OR. A%Y .NE. 1.0 ) ERROR STOP 11
      TYPE IS ( POINT_3D )
        ! "TYPE ( POINT_3D ) :: A" implied here
        PRINT *, A%X, A%Y, A%Z
      END SELECT

      END


