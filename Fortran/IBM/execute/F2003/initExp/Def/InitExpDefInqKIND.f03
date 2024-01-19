!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 03, 2006
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
!*  a reference to a specification inquiry
!*
!*  - KIND
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefInqKIND
  IMPLICIT NONE
  INTEGER :: I, J, K

  CALL ISub(1_1, NULL(), NULL(), 1_8)
  CALL LSub(.FALSE._1, NULL(), NULL(), .TRUE._8)
  CALL RSub( NULL(), NULL(), 1.0_16)
  CALL ZSub( NULL(), NULL(), (1.0_16,1.0_16))

  CONTAINS

  SUBROUTINE ISub(I1,I2,I4,I8)
  INTEGER(1)          :: I1
  INTEGER(2), POINTER :: I2
  INTEGER(4), ALLOCATABLE :: I4(:)
  INTEGER(8), OPTIONAL, INTENT(IN) :: I8

  INTEGER  :: TI1 = KIND(I1)
  INTEGER  :: TI2 = KIND(I2)
  INTEGER  :: TI4 = KIND(I4)
  INTEGER  :: TI8 = KIND(I8)

  IF ( TI1    .NE. 1     )            ERROR STOP 11
  IF ( TI2    .NE. 2     )            ERROR STOP 12
  IF ( TI4    .NE. 4     )            ERROR STOP 14
  IF ( TI8    .NE. 8     )            ERROR STOP 18

  END SUBROUTINE

  SUBROUTINE LSub(L1,L2,L4,L8)
  LOGICAL(1)                       :: L1
  LOGICAL(2), POINTER              :: L2
  LOGICAL(4), ALLOCATABLE          :: L4(:)
  LOGICAL(8), OPTIONAL, INTENT(IN) :: L8

  INTEGER  :: TL1 = KIND(L1)
  INTEGER  :: TL2 = KIND(L2)
  INTEGER  :: TL4 = KIND(L4)
  INTEGER  :: TL8 = KIND(L8)

  IF ( TL1    .NE. 1     )            ERROR STOP 21
  IF ( TL2    .NE. 2     )            ERROR STOP 22
  IF ( TL4    .NE. 4     )            ERROR STOP 24
  IF ( TL8    .NE. 8     )            ERROR STOP 28

  END SUBROUTINE

  SUBROUTINE RSub(R4,R8,R6)
  REAL(4),  POINTER              :: R4
  REAL(8),  ALLOCATABLE          :: R8(:)
  REAL(16), OPTIONAL, INTENT(IN) :: R6

  INTEGER  :: TR4 = KIND(R4)
  INTEGER  :: TR8 = KIND(R8)
  INTEGER  :: TR6 = KIND(R6)

  IF ( TR4    .NE. 4     )            ERROR STOP 34
  IF ( TR8    .NE. 8     )            ERROR STOP 38
  IF ( TR6    .NE. 16    )            ERROR STOP 36

  END SUBROUTINE

  SUBROUTINE ZSub(Z4,Z8,Z6)
  COMPLEX(4),  POINTER              :: Z4
  COMPLEX(8),  ALLOCATABLE          :: Z8(:)
  COMPLEX(16), OPTIONAL, INTENT(IN) :: Z6

  INTEGER  :: TZ4 = KIND(Z4)
  INTEGER  :: TZ8 = KIND(Z8)
  INTEGER  :: TZ6 = KIND(Z6)

  IF ( TZ4    .NE. 4     )            ERROR STOP 44
  IF ( TZ8    .NE. 8     )            ERROR STOP 48
  IF ( TZ6    .NE. 16    )            ERROR STOP 46

  END SUBROUTINE

  END



