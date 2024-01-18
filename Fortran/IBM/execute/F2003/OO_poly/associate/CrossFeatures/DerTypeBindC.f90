! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 07, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is of a derived type with bindc attribute
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM DerTypeBindC
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  TYPE, BIND(C) :: DT
    INTEGER(C_INT) :: I=1
    CHARACTER(KIND=C_CHAR) :: C="!"
  END TYPE

  TYPE(DT), TARGET :: T

  ASSOCIATE( As => T )
  ASSOCIATE( V => T )

      IF ( As%I   .NE. 1 ) STOP 31
      IF ( As%C   .NE. "!" ) STOP 32

      As%I=-1
      As%C=" "

      IF ( V%I   .NE. -1 ) STOP 41
      IF ( V%C   .NE. " " ) STOP 42

      CALL SUB(As)

      IF ( As%I   .NE.  1 ) STOP 61
      IF ( As%C   .NE. "!" ) STOP 62

  END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), target:: Arg
  TYPE(DT), POINTER:: P

  P=>Arg

  ASSOCIATE ( As => P )
    IF ( As%I   .NE. -1 ) STOP 51
    IF ( As%C   .NE. " " ) STOP 52

    As%I = 1
    As%C = "!"

  END ASSOCIATE

  END SUBROUTINE

  END


