!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 06, 2006
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
!*  If data pointer-object is of sequence derived type or a type with the BIND
!*  attribute, the dynamic type of data-target shall be that derived type.
!*
!*  -sequence
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtriPolyPtr1
  IMPLICIT NONE

  TYPE :: DT
    SEQUENCE
    CHARACTER(3) :: C="abc"
  END TYPE


  TYPE(DT),   POINTER  :: Ptr1(:,:)
  CLASS(*),   POINTER  :: Ptr2(:,:)

  TYPE(DT),   POINTER  :: Ptr5(:,:)
  CLASS(*),   POINTER  :: Ptr6(:,:)
  TYPE(DT),   POINTER  :: Ptr7(:,:)

  ALLOCATE(Ptr1(3:3, 10))
  Ptr5(1:, 1:) => Ptr1
  IF (ANY( LBOUND(Ptr5) .NE. (/1,1 /))) ERROR STOP 11
  IF (ANY( UBOUND(Ptr5) .NE. (/1,10/))) ERROR STOP 12
  IF (ANY(Ptr5%C        .NE. "abc"))    ERROR STOP 13
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr1(10, 3:3), SOURCE=DT())
  Ptr5(0:2, 0:2) => Ptr1(:,3)
  IF (ANY( LBOUND(Ptr5) .NE. (/0,0 /))) ERROR STOP 14
  IF (ANY( UBOUND(Ptr5) .NE. (/2,2 /))) ERROR STOP 15
  IF (ANY(Ptr5%C        .NE. "abc"))    ERROR STOP 16
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr1(3:3, 10))
  Ptr6(1:, 1:) => Ptr1
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) ERROR STOP 21
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) ERROR STOP 22
  Ptr7(1:, 1:) => Ptr6
  IF (ANY(Ptr7%C      .NE. "abc"))      ERROR STOP 23
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr1(10, 3:3), SOURCE=DT())
  Ptr6(0:2, 0:2) => Ptr1(:, 3)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) ERROR STOP 24
  IF (ANY( UBOUND(Ptr6) .NE. (/2,2 /))) ERROR STOP 25
  Ptr7(1:, 1:) => Ptr6
  IF (ANY(Ptr7%C      .NE. "abc"))      ERROR STOP 26
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr2(3:3, 10), SOURCE=DT("123"))
  Ptr6(1:, 1:) => Ptr2
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) ERROR STOP 31
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) ERROR STOP 32
  Ptr7(1:, 1:) => Ptr6
  IF (ANY(Ptr7%C      .NE. "123"))      ERROR STOP 33
  DEALLOCATE(Ptr2)

  ALLOCATE(Ptr2(10, 3:3), SOURCE=DT("321"))
  Ptr6(0:2, 0:2) => Ptr2(:, 3)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) ERROR STOP 34
  IF (ANY( UBOUND(Ptr6) .NE. (/2,2 /))) ERROR STOP 35
  Ptr7(1:, 1:) => Ptr6
  IF (ANY(Ptr7%C      .NE. "321"))      ERROR STOP 36
  DEALLOCATE(Ptr2)



  END


