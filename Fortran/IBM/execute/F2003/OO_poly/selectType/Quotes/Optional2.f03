! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2005
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
!*  The associating entity's optional attribute.
!*  Scalar
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM Optional2
  USE M
  IMPLICIT NONE

  INTEGER(8), TARGET   :: Int=8
  INTEGER(8), POINTER  :: IntPtr
  COMPLEX(16), TARGET   :: Cplx=(-8.0_8, 8.0_8)
  COMPLEX(16), POINTER  :: CplxPtr
  REAL(16),   TARGET   :: R=3
  REAL(16),   POINTER  :: RPtr
  LOGICAL(2), TARGET   :: L=.true.
  LOGICAL(2), POINTER  :: LPtr
  CHARACTER(8049), TARGET  :: C
  CHARACTER(8049), POINTER :: CPtr
  CHARACTER(8049)          :: Str
  TYPE(DT),   TARGET   :: DTV
  TYPE(DT),   POINTER  :: DTVPtr


  Str(:) = '!'
  C = Str

  CALL Sub()
  CALL Sub(Int)
  CALL Sub(Cplx)
  CALL Sub(R)
  CALL Sub(L)
  CALL Sub(C)
  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), TARGET, OPTIONAL  :: Arg

    IF ( .NOT. PRESENT(Arg)) RETURN

    SELECT TYPE (U => Arg)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))       ERROR STOP 30

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (INTEGER(KIND(IntPtr)))
        IntPtr => U
        IF ( U   .NE. IntPtr )      ERROR STOP 35
        IF ( KIND(U) .NE. KIND(IntPtr) ) ERROR STOP 36

      TYPE IS (COMPLEX(KIND(CplxPtr)))
        CplxPtr => U
        IF ( U   .NE. CplxPtr )     ERROR STOP 37
        IF ( KIND(U) .NE. KIND(CplxPtr) )ERROR STOP 38

      TYPE IS (REAL(KIND(RPtr)))
        RPtr => U
        IF ( U   .NE. RPtr )       ERROR STOP 37
        IF ( KIND(U) .NE. KIND(RPtr) )  ERROR STOP 38

      TYPE IS (LOGICAL(KIND(LPtr)))
        LPtr => U
        IF ( U   .NEQV. LPtr)       ERROR STOP 40
        IF ( KIND(U) .NE. KIND(LPtr) )   ERROR STOP 41

      TYPE IS (CHARACTER(*))
        CPtr => U
        IF ( U  .NE. CPtr )       ERROR STOP 42
        IF ( LEN(U) .NE. LEN(CPtr) )   ERROR STOP 43

      TYPE IS (DT)
        DTVPtr => DTV
        IF ( U%Id      .NE. DTVPtr%Id )      ERROR STOP 42
        IF ( U%GetId() .NE. DTVPtr%GetId())   ERROR STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END


