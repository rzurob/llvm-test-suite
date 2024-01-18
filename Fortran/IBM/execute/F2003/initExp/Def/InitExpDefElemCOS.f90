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
!*  -  COS
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemCOS
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL    :: r

  REAL(4),        PARAMETER  :: XR4(4,4) = 1.0
  REAL(8),        PARAMETER  :: XR8(4,4) = 1.0
  REAL(16),       PARAMETER  :: XR6(4,4) = 1.0

  COMPLEX(4),     PARAMETER  :: XZ4(4,4) = (1.0, 0.0)
  COMPLEX(8),     PARAMETER  :: XZ8(4,4) = (1.0, 0.0)
  COMPLEX(16),    PARAMETER  :: XZ6(4,4) = (1.0, 0.0)

  REAL(4),     PARAMETER  :: R4(4,4) = 0.54030231
  REAL(8),     PARAMETER  :: R8(4,4) = 0.54030231
  REAL(16),    PARAMETER  :: R6(4,4) = 0.54030231

  COMPLEX(4),  PARAMETER  :: Z4(4,4) = (0.54030231, 0.)
  COMPLEX(8),  PARAMETER  :: Z8(4,4) = (0.54030231, 0.)
  COMPLEX(16), PARAMETER  :: Z6(4,4) = (0.54030231, 0.)

  REAL(4),     PARAMETER  :: TR4(4,4) =  COS(XR4)
  REAL(8),     PARAMETER  :: TR8(4,4) =  COS(XR8)
  REAL(16),    PARAMETER  :: TR6(4,4) =  COS(XR6)

  COMPLEX(4),  PARAMETER  :: TZ4(4,4) = COS(XZ4)
  COMPLEX(8),  PARAMETER  :: TZ8(4,4) = COS(XZ8)
  COMPLEX(16), PARAMETER  :: TZ6(4,4) = COS(XZ6)


  IF (ANY(ABS(TR4 - R4 )   .GE.  1.0e-5  ) )   ERROR STOP 11
  IF (ANY(ABS(TR8 - R8 )   .GE.  1.0e-5  ) )   ERROR STOP 12
  IF (ANY(ABS(TR6 - R6 )   .GE.  1.0e-5  ) )   ERROR STOP 13

  IF (ANY(ABS(TZ4 - Z4 )   .GE.  1.0e-5  ) )   ERROR STOP 21
  IF (ANY(ABS(TZ8 - Z8 )   .GE.  1.0e-5  ) )   ERROR STOP 22
  IF (ANY(ABS(TZ6 - Z6 )   .GE.  1.0e-5  ) )   ERROR STOP 23

  END


