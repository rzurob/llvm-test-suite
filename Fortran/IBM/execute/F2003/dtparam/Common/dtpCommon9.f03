!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 18, 2007
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
!*  Data pointers that are storage associated shall have deferred the same type parameters;
!*  corresponding nondeferred type parameters shall have the same value
!*
!*  (339408/345469/347606)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K,L1,L2)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L1=4
    INTEGER, LEN  :: L2=4
    SEQUENCE
    CHARACTER(L1)  :: C1(L2)
    CHARACTER(L2),POINTER :: C3(:)
    CHARACTER(L2)  :: C2(L1)
  END TYPE

  TYPE(DT(2,7,:)), POINTER  :: Ptr0(:)
  COMMON /MyBlk/Ptr0

  END MODULE


  PROGRAM dtpCommon9
  USE M, ONLY: Ptr0, DT
  IMPLICIT NONE

  TYPE(DT(2,7,:)), POINTER  :: Ptr(:)
  COMMON /MyBlk/Ptr

  TYPE(DT(2,7,9)), TARGET  :: Tar(0:99)
  COMMON Tar

  INTEGER I, J

  DO I=0, 99
    ALLOCATE(Tar(I)%C3(9), SOURCE="123456789")
    Tar(I)%C1 = CHAR(I)
    Tar(I)%C2 = CHAR(I)
  END DO

  Ptr => Tar

  IF ( .NOT. ASSOCIATED(Ptr0, Ptr)       ) ERROR STOP 10
  IF ( ANY( LBOUND( Ptr0 ) .NE. [0]    ) ) ERROR STOP 11
  IF ( ANY( UBOUND( Ptr0 ) .NE. [99]   ) ) ERROR STOP 12

  DO I=0, 99

    IF ( ANY( LBOUND( Ptr0(I)%C1 ) .NE. [1]    ) ) ERROR STOP 21
    IF ( ANY( LBOUND( Ptr0(I)%C2 ) .NE. [1]    ) ) ERROR STOP 22
    IF ( ANY( LBOUND( Ptr0(I)%C3 ) .NE. [1]    ) ) ERROR STOP 23

    IF ( ANY( UBOUND( Ptr0(I)%C1 ) .NE. [9]    ) ) ERROR STOP 31
    IF ( ANY( UBOUND( Ptr0(I)%C2 ) .NE. [7]    ) ) ERROR STOP 32
    IF ( ANY( UBOUND( Ptr0(I)%C3 ) .NE. [9]    ) ) ERROR STOP 33

    IF ( Ptr0(I)%C1%LEN  .NE. 7  ) ERROR STOP 41
    IF ( Ptr0(I)%C2%LEN  .NE. 9  ) ERROR STOP 42
    IF ( Ptr0(I)%C3%LEN  .NE. 9  ) ERROR STOP 43

    IF ( ANY( Ptr0(I)%C1 .NE. CHAR(I)      ) ) ERROR STOP 51
    IF ( ANY( Ptr0(I)%C2 .NE. CHAR(I)      ) ) ERROR STOP 52
    IF ( ANY( Ptr0(I)%C3 .NE. "123456789"  ) ) ERROR STOP 53

  END DO

  END PROGRAM

