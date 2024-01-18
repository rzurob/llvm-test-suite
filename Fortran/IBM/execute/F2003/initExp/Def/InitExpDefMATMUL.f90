!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefMATMUL.f
!*
!*  DATE                       : Mar 28, 2006
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
!*  a reference to an tranformational intrinsic
!*
!*  - MATMUL
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpDefMATMUL
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER,            PARAMETER :: AB(2,2) =RESHAPE((/14,20,20,29/), (/2,2/))
  INTEGER,            PARAMETER :: XA(3)   =RESHAPE((/5,8,11/), (/3/))
  INTEGER,            PARAMETER :: AY(2)   =RESHAPE((/14,20/), (/2/))
  LOGICAL,            PARAMETER :: LC (2,2)=RESHAPE((/.TRUE.,.TRUE.,.FALSE.,.FALSE./), (/2,2/))

  INTEGER(1),         PARAMETER :: A11(2,3)=RESHAPE((/1,2,2,3,3,4/), (/2,3/))
  INTEGER(1),         PARAMETER :: B11(3,2)=RESHAPE((/1,2,3,2,3,4/), (/3,2/))
  INTEGER(1),         PARAMETER :: X11(2)  =RESHAPE((/1,2/), (/2/))
  INTEGER(1),         PARAMETER :: Y11(3)  =RESHAPE((/1,2,3/), (/3/))
  LOGICAL(1),         PARAMETER :: LA1(2,2)=RESHAPE((/.TRUE.,.TRUE.,.TRUE.,.TRUE./), (/2,2/))
  LOGICAL(1),         PARAMETER :: LB1(2,2)=RESHAPE((/.TRUE.,.TRUE.,.FALSE.,.FALSE./), (/2,2/))

  INTEGER(KIND(MATMUL(A11, B11))) :: T11(2,2)=MATMUL(A11, B11 )
  INTEGER(KIND(MATMUL(X11, A11))) :: T12(3)  =MATMUL(X11, A11 )
  INTEGER(KIND(MATMUL(A11, Y11))) :: T13(2)  =MATMUL(A11, Y11 )
  LOGICAL(KIND(MATMUL(LA1, LB1))) :: T14(2,2)=MATMUL(LA1, LB1 )

  REAL(8),         PARAMETER :: A81(2,3)=RESHAPE((/1,2,2,3,3,4/), (/2,3/))
  REAL(8),         PARAMETER :: B81(3,2)=RESHAPE((/1,2,3,2,3,4/), (/3,2/))
  REAL(8),         PARAMETER :: X81(2)  =RESHAPE((/1,2/), (/2/))
  REAL(8),         PARAMETER :: Y81(3)  =RESHAPE((/1,2,3/), (/3/))
  LOGICAL(8),      PARAMETER :: LA8(2,2)=RESHAPE((/.TRUE.,.TRUE.,.TRUE.,.TRUE./), (/2,2/))
  LOGICAL(8),      PARAMETER :: LB8(2,2)=RESHAPE((/.TRUE.,.TRUE.,.FALSE.,.FALSE./), (/2,2/))

  INTEGER(KIND(MATMUL(A81, B11))) :: T81(2,2)=MATMUL(A81, B11 )
  INTEGER(KIND(MATMUL(X81, A11))) :: T82(3)  =MATMUL(X81, A11 )
  INTEGER(KIND(MATMUL(A81, Y11))) :: T83(2)  =MATMUL(A81, Y11 )
  LOGICAL(KIND(MATMUL(LA8, LB1))) :: T84(2,2)=MATMUL(LA8, LB1 )

  COMPLEX(4),      PARAMETER :: Z4(2,2)=RESHAPE((/(1,1),(1,1),(1,1),(1,1)/), (/2,2/))
  COMPLEX(8),      PARAMETER :: Z8(2,2)=RESHAPE((/(1,1),(1,1),(1,1),(1,1)/), (/2,2/))
  LOGICAL(4),      PARAMETER :: LA4(2,2)=RESHAPE((/.TRUE.,.TRUE.,.TRUE.,.TRUE./), (/2,2/))
  LOGICAL(4),      PARAMETER :: LB4(2,2)=RESHAPE((/.TRUE.,.TRUE.,.FALSE.,.FALSE./), (/2,2/))

  COMPLEX(KIND(MATMUL(Z4, Z4)))  :: TZ4(2,2) =MATMUL(Z4, Z4)
  COMPLEX(KIND(MATMUL(Z4, Z8)))  :: TZ8(2,2) =MATMUL(Z4, Z8 )
  LOGICAL(KIND(MATMUL(LA4,LB4))) :: T44(2,2) =MATMUL(LA4, LB4 )


  IF (KIND(T11)   .NE.  1  )         STOP 11
  IF (ANY( T11    .NE.  AB ))        STOP 12
  IF (KIND(T12)   .NE.  1  )         STOP 13
  IF (ANY( T12    .NE.  XA ))        STOP 14
  IF (KIND(T13)   .NE.  1  )         STOP 15
  IF (ANY( T13    .NE.  AY ))        STOP 16
  IF (KIND(T14)   .NE.  1  )         STOP 17
  IF (ANY( T14    .NEQV. LC ))       STOP 18

  IF (KIND(T81)   .NE.  8  )         STOP 81
  IF (ANY( T81    .NE.  AB ))        STOP 82
  IF (KIND(T82)   .NE.  8  )         STOP 83
  IF (ANY( T82    .NE.  XA ))        STOP 84
  IF (KIND(T83)   .NE.  8  )         STOP 85
  IF (ANY( T83    .NE.  AY ))        STOP 86
  IF (KIND(T84)   .NE.  8  )         STOP 87
  IF (ANY( T84    .NEQV. LC ))       STOP 88

  IF (KIND(TZ4)   .NE.  4  )         STOP 51
  IF (ANY(TZ4     .NE.  (0,4)))      STOP 52
  IF (KIND(TZ8)   .NE.  8  )         STOP 53
  IF (ANY(TZ8     .NE.  (0,4)))      STOP 54
  IF (KIND(T44)   .NE.  4  )         STOP 55
  IF (ANY( T44    .NEQV. LC ))       STOP 56

  END



