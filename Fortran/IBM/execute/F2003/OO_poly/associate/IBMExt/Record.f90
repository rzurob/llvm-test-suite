! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 05, 2005
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
!*    The selector is of record structure
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SltRecord
  IMPLICIT NONE

  INTEGER :: Pi

  STRUCTURE /S1/
    STRUCTURE /S2/ A
      INTEGER :: I=1
      INTEGER :: Pi=-3
      PARAMETER (Pi=3)  ! IBM Ext
    END STRUCTURE
    STRUCTURE B
      INTEGER :: J=2
    END STRUCTURE
  END STRUCTURE

 RECORD /S1/ R1
 RECORD /S2/ R2

  ASSOCIATE ( As1 => R1, As2 => R2  )
    IF ( As1.A%Pi .NE. -3  )     STOP 20
    IF ( As1.A%I  .NE. 1  )      STOP 21
    IF ( As1.B%J  .NE. 2  )      STOP 22

    ASSOCIATE ( As1 => As1.A, As2 => As2.I, As3 => As1.B )
      IF ( As1%I  .NE. 1  )      STOP 23
      IF ( As2    .NE. 1  )      STOP 24
      IF ( As3%J  .NE. 2  )      STOP 25

      As1%I = -1
      As3%J = -2

      IF ( As1%I  .NE. -1  )      STOP 26
      IF ( As3%J  .NE. -2  )      STOP 27
    END ASSOCIATE

    IF ( As1.A%I  .NE. -1  )      STOP 31
    IF ( As1.B%J  .NE. -2  )      STOP 32
    IF ( R1.A%I   .NE. -1  )      STOP 33
    IF ( R1.B%J   .NE. -2  )      STOP 34

  END ASSOCIATE

  END
