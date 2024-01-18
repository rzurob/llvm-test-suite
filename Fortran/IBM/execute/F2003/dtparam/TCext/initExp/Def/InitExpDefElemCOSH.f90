! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemCOSH.f
! opt variations: -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemCOSH.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 07, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
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

  TYPE :: DT(K1,K2,K3)    ! (4,8,16) 
    INTEGER, KIND :: K1,K2,K3
    REAL(K1)      :: TR4(4,4) = COSH(XR4)
    REAL(K2)      :: TR8(4,4) = COSH(XR8)
    REAL(K3)      :: TR6(4,4) = COSH(XR6)

    COMPLEX(K1)   :: TZ4(4,4) = COSH(REAL(XZ4, KIND=4))
    COMPLEX(K2)   :: TZ8(4,4) = COSH(REAL(XZ8, KIND=8))
    COMPLEX(K3)   :: TZ6(4,4) = COSH(REAL(XZ6, KIND=16))
  END TYPE

  TYPE(DT(4,8,16)) :: T

  IF (ANY(ABS(T%TR4 - R4 )   .GE.  1.0e-5  ) )   STOP 11 
  IF (ANY(ABS(T%TR8 - R8 )   .GE.  1.0e-5  ) )   STOP 12 
  IF (ANY(ABS(T%TR6 - R6 )   .GE.  1.0e-5  ) )  STOP 13 

  IF (ANY(ABS(T%TZ4 - Z4 )   .GE.  1.0e-5  ) )   STOP 21 
  IF (ANY(ABS(T%TZ8 - Z8 )   .GE.  1.0e-5  ) )   STOP 22 
  IF (ANY(ABS(T%TZ6 - Z6 )   .GE.  1.0e-5  ) )  STOP 23 

  END

 
