!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 05, 2006
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
!*  If data-pointer-object is polymorphic (5.1.1.2), it assumes the dynamic type
!*  of data-target.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtriPolyPtr
  IMPLICIT NONE

  TYPE :: DT0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT
  END TYPE

  CLASS(DT0), POINTER  :: Ptr1(:,:)
  CLASS(DT),  POINTER  :: Ptr2(:,:)
  CLASS(*),   POINTER  :: Ptr3(:,:)

  CLASS(DT0), POINTER  :: Ptr4(:,:)
  CLASS(DT),  POINTER  :: Ptr5(:,:)
  CLASS(*),   POINTER  :: Ptr6(:,:)

  ALLOCATE(Ptr1(3:3, 10))
  Ptr4(1:, 1:) => Ptr1
  IF (ANY( LBOUND(Ptr4) .NE. (/1,1 /))) ERROR STOP 11
  IF (ANY( UBOUND(Ptr4) .NE. (/1,10/))) ERROR STOP 12
  IF (.NOT. SAME_TYPE_AS(Ptr4, Ptr1))   ERROR STOP 112
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr1(31, 3:3), SOURCE=DT())
  Ptr4(0:9, 0:2) => Ptr1(:,3)
  IF (ANY( LBOUND(Ptr4) .NE. (/0,0 /))) ERROR STOP 13
  IF (ANY( UBOUND(Ptr4) .NE. (/9,2 /))) ERROR STOP 14
  IF (.NOT. SAME_TYPE_AS(Ptr4, Ptr1))   ERROR STOP 114
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr1(3:3, 10), SOURCE=DT())
  Ptr6(1:, 1:) => Ptr1
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) ERROR STOP 15
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) ERROR STOP 16
  IF (.NOT. SAME_TYPE_AS(Ptr4, Ptr1))   ERROR STOP 116
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr1(30, 3:3))
  Ptr6(0:9, 0:2) => Ptr1(:,3)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) ERROR STOP 17
  IF (ANY( UBOUND(Ptr6) .NE. (/9,2 /))) ERROR STOP 18
  IF (.NOT. SAME_TYPE_AS(Ptr6, Ptr1))   ERROR STOP 118
  DEALLOCATE(Ptr1)

  ALLOCATE(Ptr2(3:3, 10))
  Ptr5(1:, 1:) => Ptr2
  IF (ANY( LBOUND(Ptr5) .NE. (/1,1 /))) ERROR STOP 21
  IF (ANY( UBOUND(Ptr5) .NE. (/1,10/))) ERROR STOP 22
  DEALLOCATE(Ptr2)

  ALLOCATE(Ptr2(30, -3:3))
  Ptr5(0:9, 0:2) => Ptr2(:,0)
  IF (ANY( LBOUND(Ptr5) .NE. (/0,0 /))) ERROR STOP 23
  IF (ANY( UBOUND(Ptr5) .NE. (/9,2 /))) ERROR STOP 24
  DEALLOCATE(Ptr2)

  ALLOCATE(Ptr2(3:3, 10))
  Ptr6(1:, 1:) => Ptr2
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) ERROR STOP 25
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) ERROR STOP 26
  IF (.NOT. SAME_TYPE_AS(Ptr6, Ptr2))   ERROR STOP 126
  DEALLOCATE(Ptr2)

  ALLOCATE(Ptr2(30, 0:0))
  Ptr6(0:9, 0:2) => Ptr2(::1,0)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) ERROR STOP 27
  IF (ANY( UBOUND(Ptr6) .NE. (/9,2 /))) ERROR STOP 28
  IF (.NOT. SAME_TYPE_AS(Ptr6, Ptr2))   ERROR STOP 128
  DEALLOCATE(Ptr2)

  ALLOCATE(Ptr3(3:3, 10), SOURCE=DT0())
  Ptr6(1:, 1:) => Ptr3
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) ERROR STOP 31
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) ERROR STOP 32
  IF (.NOT. SAME_TYPE_AS(Ptr6, Ptr3))   ERROR STOP 132
  DEALLOCATE(Ptr3)

  ALLOCATE(Ptr3(30, 0), SOURCE=DT())
  Ptr6(0:9, 0:2) => Ptr3(:,0)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) ERROR STOP 33
  IF (ANY( UBOUND(Ptr6) .NE. (/9,2 /))) ERROR STOP 34
  IF (.NOT. SAME_TYPE_AS(Ptr6, Ptr3))   ERROR STOP 134
  DEALLOCATE(Ptr3)


  END


