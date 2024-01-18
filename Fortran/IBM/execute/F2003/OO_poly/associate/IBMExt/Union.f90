! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Union.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Union
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
    IF ( As.I .NE.  1  )    STOP 20
    IF ( As.J .NE. -1  )    STOP 21

    IF ( As.L .NE.  1  )    STOP 22
    IF ( As.M .NE. -1  )    STOP 23

    ASSOCIATE ( As1 => As.I, As2 => As.J )

      IF ( As1 .NE.  1  )   STOP 24
      IF ( As2 .NE. -1  )   STOP 25

      As1 = -1
      As2 = 1

    END ASSOCIATE

    IF ( As.I .NE. -1  )    STOP 30
    IF ( As.J .NE.  1  )    STOP 31

    IF ( As.L .NE. -1  )    STOP 32
    IF ( As.M .NE.  1  )    STOP 33

  END ASSOCIATE

  IF ( R.I .NE. -1  )      STOP 40
  IF ( R.J .NE.  1  )      STOP 41

  IF ( R.L .NE. -1  )      STOP 50
  IF ( R.M .NE.  1  )      STOP 51

  END
