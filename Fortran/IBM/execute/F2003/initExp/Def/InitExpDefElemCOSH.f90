!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 07, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -  COSH
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemCOSH
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL    :: r

  REAL(4),        PARAMETER  :: XR4(4,4) = 1.0
  REAL(8),        PARAMETER  :: XR8(4,4) = 1.0
  REAL(16),       PARAMETER  :: XR6(4,4) = 1.0

  COMPLEX(4),     PARAMETER  :: XZ4(4,4) = (1.0, 0.0)
  COMPLEX(8),     PARAMETER  :: XZ8(4,4) = (1.0, 0.0)
  COMPLEX(16),    PARAMETER  :: XZ6(4,4) = (1.0, 0.0)

  REAL(4),     PARAMETER  :: R4(4,4) = 1.5430806
  REAL(8),     PARAMETER  :: R8(4,4) = 1.5430806
  REAL(16),    PARAMETER  :: R6(4,4) = 1.5430806

  COMPLEX(4),  PARAMETER  :: Z4(4,4) = (1.5430806, 0.)
  COMPLEX(8),  PARAMETER  :: Z8(4,4) = (1.5430806, 0.)
  COMPLEX(16), PARAMETER  :: Z6(4,4) = (1.5430806, 0.)

  TYPE :: DT
    REAL(4)     :: TR4(4,4) = COSH(XR4)
    REAL(8)     :: TR8(4,4) = COSH(XR8)
    REAL(16)    :: TR6(4,4) = COSH(XR6)

    COMPLEX(4)  :: TZ4(4,4) = COSH(REAL(XZ4, KIND=4))
    COMPLEX(8)  :: TZ8(4,4) = COSH(REAL(XZ8, KIND=8))
    COMPLEX(16) :: TZ6(4,4) = COSH(REAL(XZ6, KIND=16))
  END TYPE

  TYPE(DT) :: T

  IF (ANY(ABS(T%TR4 - R4 )   .GE.  1.0e-5  ) )   ERROR STOP 11
  IF (ANY(ABS(T%TR8 - R8 )   .GE.  1.0e-5  ) )   ERROR STOP 12
  IF (ANY(ABS(T%TR6 - R6 )   .GE.  1.0e-5  ) )  ERROR STOP 13

  IF (ANY(ABS(T%TZ4 - Z4 )   .GE.  1.0e-5  ) )   ERROR STOP 21
  IF (ANY(ABS(T%TZ8 - Z8 )   .GE.  1.0e-5  ) )   ERROR STOP 22
  IF (ANY(ABS(T%TZ6 - Z6 )   .GE.  1.0e-5  ) )  ERROR STOP 23

  END


