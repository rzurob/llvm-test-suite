!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 13, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- The common statement
!*
!*     C588 (R558) A common-block-object shall not be a dummy argument, an allocatable variable, a
!*     derived-type object with an ultimate component that is allocatable, an automatic object, a
!*     function name, an entry name, a variable with the BIND attribute, or a result name.
!*
!*   -- Test pointer components
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K
  END TYPE

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C(L)=CHAR(48+K)
  END TYPE

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)=K
  END TYPE

  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL(K)    :: A(L)=.TRUE.
  END TYPE

  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX(K)    :: Z(L)=(K,-K)
  END TYPE

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    TYPE(DT_R(K,L)), POINTER  :: R
    TYPE(DT_C(K,L)), POINTER  :: C
    TYPE(DT_I(K,L)), POINTER  :: I
    TYPE(DT_L(K,L)), POINTER  :: A
    TYPE(DT_Z(K,L)), POINTER  :: Z
  END TYPE

  END MODULE

  PROGRAM dtpCommonC588
  USE M

  TYPE(DT(8,7)), POINTER :: Ptr
  TYPE(DT(8,7)), TARGET  :: Tar

  COMMON /BLK/ Ptr, Tar

  CALL ExtSub()

  IF ( .NOT. ASSOCIATED(Ptr, Tar) ) ERROR STOP 11

  IF ( SIZE(Tar%R%R) .NE. 7   ) ERROR STOP 21
  IF ( ANY (Tar%R%R  .NE. 8 ) ) ERROR STOP 22

  IF ( SIZE(Tar%C%C) .NE. 7         ) ERROR STOP 31
  IF ( ANY (Tar%C%C  .NE. CHAR(8) ) ) ERROR STOP 32

  IF ( SIZE(Tar%I%I) .NE. 7   ) ERROR STOP 41
  IF ( ANY (Tar%I%I  .NE. 8 ) ) ERROR STOP 42

  IF ( SIZE(Tar%A%A) .NE.   7        ) ERROR STOP 51
  IF ( ANY (Tar%A%A  .NEQV. .TRUE. ) ) ERROR STOP 52

  IF ( SIZE(Tar%Z%Z) .NE. 7        ) ERROR STOP 61
  IF ( ANY (Tar%Z%Z  .NE. (8,-8) ) ) ERROR STOP 62

  END

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT(8,7)), POINTER :: Ptr
  TYPE(DT(8,7)), TARGET  :: Tar

  COMMON /BLK/ Ptr, Tar

  ALLOCATE(Tar%R)
  ALLOCATE(Tar%C)
  ALLOCATE(Tar%I)
  ALLOCATE(Tar%A)
  ALLOCATE(Tar%Z)


  Tar%R%R = 8
  Tar%C%C = CHAR(8)
  Tar%I%I = 8
  Tar%A%A = .TRUE.
  Tar%Z%Z = (8,-8)

  Ptr => Tar

  END


