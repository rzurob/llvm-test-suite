!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  type-spec in select type
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpeSltTyp1

  CLASS(*), ALLOCATABLE :: R4(:)
  CLASS(*), ALLOCATABLE :: R8(:)
  CLASS(*), ALLOCATABLE :: R6(:)

  CLASS(*), POINTER  :: Z4(:)
  CLASS(*), POINTER  :: Z8(:)
  CLASS(*), POINTER  :: Z6(:)

  ALLOCATE(R4(128), SOURCE=-4._4)
  ALLOCATE(R8(128), SOURCE=-8._8)
  ALLOCATE(R6(128), SOURCE=-6._16)

  ALLOCATE(Z4(128), SOURCE=(-4._4, 0._4))
  ALLOCATE(Z8(128), SOURCE=(-8._8, 0._8))
  ALLOCATE(Z6(128), SOURCE=(-6._16, 0._16))

  SELECT TYPE ( R4 )
  TYPE IS (REAL(KIND=KIND([-4._4])))
    IF ( KIND(R4) .NE. 4     ) STOP 21
    IF ( SIZE(R4) .NE. 128   ) STOP 22
    IF ( ANY( R4  .NE. -4_4 )) STOP 23
  CLASS DEFAULT
    STOP 24
  END SELECT

  SELECT TYPE ( R8 )
  TYPE IS (REAL(KIND=KIND([-8._8])))
    IF ( KIND(R8) .NE. 8     ) STOP 31
    IF ( SIZE(R8) .NE. 128   ) STOP 32
    IF ( ANY( R8  .NE. -8_8 )) STOP 33
  CLASS DEFAULT
    STOP 34
  END SELECT

  SELECT TYPE ( R6 )
  TYPE IS (REAL(KIND=KIND([-16._16])))
    IF ( KIND(R6) .NE. 16    ) STOP 41
    IF ( SIZE(R6) .NE. 128   ) STOP 42
    IF ( ANY( R6  .NE. -6._16))STOP 43
  CLASS DEFAULT
    STOP 44
  END SELECT


  SELECT TYPE ( Z4 )
  TYPE IS (COMPLEX(KIND=KIND([(-4._4,0._4)])))
    IF ( KIND(Z4) .NE. 4     ) STOP 51
    IF ( SIZE(Z4) .NE. 128   ) STOP 52
    IF ( ANY( Z4  .NE. (-4._4,0._4) )) STOP 53
  CLASS DEFAULT
    STOP 54
  END SELECT

  SELECT TYPE ( Z8 )
  TYPE IS (COMPLEX(KIND=KIND([(-8._8,0._8)])))
    IF ( KIND(Z8) .NE. 8     ) STOP 61
    IF ( SIZE(Z8) .NE. 128   ) STOP 62
    IF ( ANY( Z8  .NE. (-8._8,0._8) )) STOP 63
  CLASS DEFAULT
    STOP 64
  END SELECT

  SELECT TYPE ( Z6 )
  TYPE IS (COMPLEX(KIND=KIND([(-6._16,0._16)])))
    IF ( KIND(Z6) .NE. 16    ) STOP 71
    IF ( SIZE(Z6) .NE. 128   ) STOP 72
    IF ( ANY( Z6  .NE. (-6._16,0._16)))STOP 73
  CLASS DEFAULT
    STOP 74
  END SELECT




  END
