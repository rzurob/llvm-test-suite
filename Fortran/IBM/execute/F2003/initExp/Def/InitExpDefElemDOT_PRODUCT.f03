!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 27, 2006
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
!*  a reference to a transformational  intrinsic
!*
!*  - DOT_PRODUCT
!*  (318833)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpDefElemDOT_PRODUCT
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),   PARAMETER :: I1(127)=(/(1, I=1,127)/)
  INTEGER(2),   PARAMETER :: I2(127)=(/(1, I=1,127)/)
  INTEGER(4),   PARAMETER :: I4(127)=(/(1, I=1,127)/)
  INTEGER(8),   PARAMETER :: I8(127)=(/(1, I=1,127)/)

  LOGICAL(1),   PARAMETER :: L1(127)=(/(.TRUE., I=1,127)/)
  LOGICAL(2),   PARAMETER :: L2(127)=(/(.TRUE., I=1,127)/)
  LOGICAL(4),   PARAMETER :: L4(127)=(/(.TRUE., I=1,127)/)
  LOGICAL(8),   PARAMETER :: L8(127)=(/(.TRUE., I=1,127)/)

  LOGICAL(1),   PARAMETER :: L10(1:0)=.TRUE.

  REAL(4),      PARAMETER :: R4(127)=(/(1, I=1,127)/)
  REAL(8),      PARAMETER :: R8(127)=(/(1, I=1,127)/)
  REAL(16),     PARAMETER :: R16(127)=(/(1, I=1,127)/)

  COMPLEX(4),   PARAMETER :: C4(127)=(/(1, I=1,127)/)
  COMPLEX(8),   PARAMETER :: C8(127)=(/(1, I=1,127)/)
  COMPLEX(16),  PARAMETER :: C16(127)=(/(1, I=1,127)/)

  INTEGER(KIND(DOT_PRODUCT(I1, I1))) :: TI0(DOT_PRODUCT(I1, I1 ))=DOT_PRODUCT(I1(1:0), I1(1:0))

  INTEGER(KIND(DOT_PRODUCT(I1, I1))) :: TI1(DOT_PRODUCT(I1, I1))=DOT_PRODUCT(I1, I1)
  INTEGER(KIND(DOT_PRODUCT(I1, I8))) :: TI8(DOT_PRODUCT(I8, I1))=DOT_PRODUCT(I8, I1)

  REAL(KIND(DOT_PRODUCT(R4, I8)))    :: TR4(INT(DOT_PRODUCT(R4,   I8)))=DOT_PRODUCT(R4, I8)
  REAL(KIND(DOT_PRODUCT(R16, i8)))   :: TR16(INT(DOT_PRODUCT(R16, I8)))=DOT_PRODUCT(R16, I8)

  COMPLEx(KIND(DOT_PRODUCT(R4, I8))) :: TC4(INT(DOT_PRODUCT(I4,   I2)))=DOT_PRODUCT(R4, I8)
  COMPLEx(KIND(DOT_PRODUCT(R16, I8))):: TC16(INT(DOT_PRODUCT(I1, I8)))=DOT_PRODUCT(R16, I8)

  LOGICAL(KIND(DOT_PRODUCT(L1, L4))) :: TL1(DOT_PRODUCT(I1, I4))=DOT_PRODUCT(L1, L4)
  LOGICAL(KIND(DOT_PRODUCT(L2, L8))) :: TL8(DOT_PRODUCT(I8, I2))=DOT_PRODUCT(L8, L2)


  IF (KIND(TI0)   .NE.   1   )        ERROR STOP 8
  IF (SIZE(TI0)   .NE.   127 )        ERROR STOP 9
  IF (ANY( TI0    .NE.   0   ))       ERROR STOP 10

  IF (KIND(TI1)   .NE.   1   )        ERROR STOP 21
  IF (SIZE(TI1)   .NE.   127 )        ERROR STOP 22
  IF (ANY( TI1    .NE.   127 ))       ERROR STOP 23

  IF (KIND(TI8)   .NE.   8   )        ERROR STOP 31
  IF (SIZE(TI8)   .NE.   127 )        ERROR STOP 32
  IF (ANY( TI8    .NE.   127 ))       ERROR STOP 33

  IF (KIND(TR4)   .NE.   4   )        ERROR STOP 41
  IF (SIZE(TR4)   .NE.   127 )        ERROR STOP 42
  IF (ANY( TR4    .NE.   127 ))       ERROR STOP 43

  IF (KIND(TR16)  .NE.   16   )       ERROR STOP 51
  IF (SIZE(TR16)  .NE.   127 )        ERROR STOP 52
  IF (ANY( TR16   .NE.   127 ))       ERROR STOP 53

  IF (KIND(TC4)   .NE.   4   )        ERROR STOP 61
  IF (SIZE(TC4)   .NE.   127 )        ERROR STOP 62
  IF (ANY( TC4    .NE.   (127., 0.0)))ERROR STOP 63

  IF (KIND(TC16)  .NE.   16   )       ERROR STOP 71
  IF (SIZE(TC16)  .NE.   127 )        ERROR STOP 72
  IF (ANY( TC16   .NE.   (127., 0.0)))ERROR STOP 73

  IF (KIND(TL1)   .NE.   4   )        ERROR STOP 81
  IF (SIZE(TL1)   .NE.   127 )        ERROR STOP 82
  IF (ANY( TL1   .NEQV. .TRUE. ))     ERROR STOP 83

  IF (KIND(TL8)   .NE.   8   )        ERROR STOP 91
  IF (SIZE(TL8)   .NE.   127 )        ERROR STOP 92
  IF (ANY( TL8   .NEQV. .TRUE. ))     ERROR STOP 93


  END



