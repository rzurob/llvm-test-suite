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
!*    The selector is of record structure with union & map
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Union
  IMPLICIT NONE

  STRUCTURE /S1/

    UNION
      MAP
        INTEGER :: I=1
        REAL    :: J=-1
      END MAP

      MAP
        INTEGER :: L
        REAL    :: M
      END MAP
    END UNION

  END STRUCTURE

  RECORD /S1/ R

  ASSOCIATE ( As => R  )
    IF ( As.I .NE.  1  )    ERROR STOP 20
    IF ( As.J .NE. -1  )    ERROR STOP 21

    IF ( As.L .NE.  1  )    ERROR STOP 22
    IF ( As.M .NE. -1  )    ERROR STOP 23

    ASSOCIATE ( As1 => As.I, As2 => As.J )

      IF ( As1 .NE.  1  )   ERROR STOP 24
      IF ( As2 .NE. -1  )   ERROR STOP 25

      As1 = -1
      As2 = 1

    END ASSOCIATE

    IF ( As.I .NE. -1  )    ERROR STOP 30
    IF ( As.J .NE.  1  )    ERROR STOP 31

    IF ( As.L .NE. -1  )    ERROR STOP 32
    IF ( As.M .NE.  1  )    ERROR STOP 33

  END ASSOCIATE

  IF ( R.I .NE. -1  )      ERROR STOP 40
  IF ( R.J .NE.  1  )      ERROR STOP 41

  IF ( R.L .NE. -1  )      ERROR STOP 50
  IF ( R.M .NE.  1  )      ERROR STOP 51

  END
