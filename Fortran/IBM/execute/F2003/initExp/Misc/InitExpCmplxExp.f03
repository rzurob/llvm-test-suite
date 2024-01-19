!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpCmplxExp.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER CmplxExpTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Complex exponentialtion
!*
!*  (325913)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpCmplxExp

  LOGICAL precision_x8
  LOGICAL precision_x6
  LOGICAL precision_x3

  COMPLEX(4),  PARAMETER :: A(128)=(1.0, 0.)
  COMPLEX(8),  PARAMETER :: B(128)=(0., 1.)
  COMPLEX(16), PARAMETER :: C(128)=(1., 1.)

  COMPLEX(KIND=4)   :: Z4(128)= A**B
  COMPLEX(KIND=8)   :: Z8(128)= (-A)**C
  COMPLEX(KIND=16)  :: Z6(128)= (-B)**(-C)

  COMPLEX(KIND=4)   :: RZ4(128)
  COMPLEX(KIND=8)   :: RZ8(128)
  COMPLEX(KIND=16)  :: RZ6(128)

  RZ4 =  A**B
  RZ8 = (-A)**C
  RZ6 = (-B)**(-C)

  DO I=1, 128
    IF ( .NOT. precision_x8(Z4(i), RZ4(i) ) ) ERROR STOP 11
    IF ( .NOT. precision_x6(Z8(i), RZ8(i) ) ) ERROR STOP 12
    IF ( .NOT. precision_x3(Z6(i), RZ6(i) ) ) ERROR STOP 13
  END DO

  END



