!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 09, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  BIND(C)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  USE ISO_C_BINDING

  INTEGER(C_INT),           TARGET, BIND(C, NAME="I") :: I(10,10)=-1
  REAL(C_FLOAT),            TARGET, BIND(C, NAME="F") :: F(10,10)=-1.0
  CHARACTER(C_CHAR),        TARGET, BIND(C, NAME="C") :: C(10,10)="1"
  LOGICAL(C_BOOL),          TARGET, BIND(C, NAME="L") :: L(10,10)=.TRUE.
  COMPLEX(C_FLOAT_COMPLEX), TARGET, BIND(C, NAME="X") :: X(10,10)=(1.0,-1.0)

  INTEGER(C_INT),           POINTER :: IPtr(:,:)
  REAL(C_FLOAT),            POINTER :: FPtr(:,:)
  CHARACTER(C_CHAR),        POINTER :: CPtr(:,:)
  LOGICAL(C_BOOL),          POINTER :: LPtr(:,:)
  COMPLEX(C_FLOAT_COMPLEX), POINTER :: XPtr(:,:)

  TYPE, BIND(C) :: DT
    INTEGER(C_INT)     :: I=-1
    CHARACTER(C_CHAR)  :: C="1"
  END TYPE

  TYPE(DT), BIND(C, NAME="T"), TARGET, SAVE :: T(10, 10)
  TYPE(DT), POINTER :: TPtr(:,:)

  END MODULE


  PROGRAM dataPtrBindC
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: PTemp(:,:)

  IPtr(0:, 0:) => I
  IF (.NOT. ASSOCIATED(IPtr, I))                    STOP 11
  IF (ANY( LBOUND(IPtr)         .NE. (/0, 0 /)))    STOP 12
  IF (ANY( UBOUND(IPtr)         .NE. (/9, 9 /)))    STOP 13
  IF (ANY( IPtr                 .NE.   -1))         STOP 14

  FPtr(0:, 0:) => F
  IF (.NOT. ASSOCIATED(FPtr, F))                    STOP 21
  IF (ANY( LBOUND(FPtr)         .NE. (/0, 0 /)))    STOP 22
  IF (ANY( UBOUND(FPtr)         .NE. (/9, 9 /)))    STOP 23
  IF (ANY( FPtr                 .NE. -1.0))         STOP 24

  CPtr(0:, 0:) => C
  IF (.NOT. ASSOCIATED(CPtr, C))                    STOP 31
  IF (ANY( LBOUND(CPtr)         .NE. (/0, 0 /)))    STOP 32
  IF (ANY( UBOUND(CPtr)         .NE. (/9, 9 /)))    STOP 33
  IF (ANY( CPtr                 .NE. "1" ))         STOP 34

  LPtr(0:, 0:) => L
  IF (.NOT. ASSOCIATED(LPtr, L))                    STOP 41
  IF (ANY( LBOUND(LPtr)         .NE. (/0, 0 /)))    STOP 42
  IF (ANY( UBOUND(LPtr)         .NE. (/9, 9 /)))    STOP 43
  IF (ANY( LPtr                 .NEQV..TRUE.))      STOP 44

  XPtr(0:, 0:) => X
  IF (.NOT. ASSOCIATED(XPtr, X))                    STOP 51
  IF (ANY( LBOUND(XPtr)         .NE. (/0, 0 /)))    STOP 52
  IF (ANY( UBOUND(XPtr)         .NE. (/9, 9 /)))    STOP 53
  IF (ANY( XPtr                 .NE. (1.0,-1.0)))   STOP 54

  TPtr(0:, 0:) => T
  IF (.NOT. ASSOCIATED(TPtr, T))                    STOP 61
  IF (ANY( LBOUND(TPtr)         .NE. (/0, 0 /)))    STOP 62
  IF (ANY( UBOUND(TPtr)         .NE. (/9, 9 /)))    STOP 63
  IF (ANY( TPtr%I               .NE.   -1))         STOP 64
  IF (ANY( TPtr%C               .NE.  "1"))         STOP 64
  PTemp(1:,1:) => TPtr
  TPtr(0:, 0:) => PTemp
  IF (ANY( TPtr%I               .NE.   -1))         STOP 164
  IF (ANY( TPtr%C               .NE.  "1"))         STOP 164


  IPtr(0:9, 0:0) => I(:,1)
  IF (.NOT. ASSOCIATED(IPtr, I(:,1:1)))             STOP 111
  IF (ANY( LBOUND(IPtr)         .NE. (/0, 0 /)))    STOP 112
  IF (ANY( UBOUND(IPtr)         .NE. (/9, 0 /)))    STOP 113
  IF (ANY( IPtr                 .NE.   -1))         STOP 114

  FPtr(0:9, 0:0) => F(:,1)
  IF (.NOT. ASSOCIATED(FPtr, F(:,1:1)))             STOP 121
  IF (ANY( LBOUND(FPtr)         .NE. (/0, 0 /)))    STOP 122
  IF (ANY( UBOUND(FPtr)         .NE. (/9, 0 /)))    STOP 123
  IF (ANY( FPtr                 .NE. -1.0))         STOP 124

  CPtr(0:9, 0:0) => C(:,1)
  IF (.NOT. ASSOCIATED(CPtr, C(:,1:1)))             STOP 131
  IF (ANY( LBOUND(CPtr)         .NE. (/0, 0 /)))    STOP 132
  IF (ANY( UBOUND(CPtr)         .NE. (/9, 0 /)))    STOP 133
  IF (ANY( CPtr                 .NE. "1" ))         STOP 134

  LPtr(0:9, 0:0) => L(:,1)
  IF (.NOT. ASSOCIATED(LPtr, L(:,1:1)))             STOP 141
  IF (ANY( LBOUND(LPtr)         .NE. (/0, 0 /)))    STOP 142
  IF (ANY( UBOUND(LPtr)         .NE. (/9, 0 /)))    STOP 143
  IF (ANY( LPtr                 .NEQV..TRUE.))      STOP 144

  XPtr(0:9, 0:0) => X(:,1)
  IF (.NOT. ASSOCIATED(XPtr, X(:,1:1)))             STOP 151
  IF (ANY( LBOUND(XPtr)         .NE. (/0, 0 /)))    STOP 152
  IF (ANY( UBOUND(XPtr)         .NE. (/9, 0 /)))    STOP 153
  IF (ANY( XPtr                 .NE. (1.0,-1.0)))   STOP 154

  TPtr(0:9, 0:0) => T(:,1)
  IF (.NOT. ASSOCIATED(TPtr, T(:,1:1)))             STOP 161
  IF (ANY( LBOUND(TPtr)         .NE. (/0, 0 /)))    STOP 162
  IF (ANY( UBOUND(TPtr)         .NE. (/9, 0 /)))    STOP 163
  IF (ANY( TPtr%I               .NE.   -1))         STOP 164
  IF (ANY( TPtr%C               .NE.  "1"))         STOP 164
  PTemp(1:10,1:1) => TPtr(:,0)
  TPtr(0:9, 0:0) => PTemp(:,1)
  IF (ANY( TPtr%I               .NE.   -1))         STOP 165
  IF (ANY( TPtr%C               .NE.  "1"))         STOP 166



  END


