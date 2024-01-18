!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtriPolyPtr2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 06, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
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
!*  If data pointer-object is of sequence derived type or a type with the BIND 
!*  attribute, the dynamic type of data-target shall be that derived type.
!*  
!*  -bind(c)
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  USE ISO_C_BINDING

  TYPE, BIND(C) :: DT
    INTEGER(C_SHORT) :: I=-1 
  END TYPE

  TYPE(DT),   TARGET, BIND(C), SAVE  :: Ptr1(3:3, 10)
  CLASS(*),   POINTER         :: Ptr2(:,:)

  TYPE(DT),   POINTER   :: Ptr5(:,:)
  CLASS(*),   POINTER   :: Ptr6(:,:)
  TYPE(DT),   POINTER   :: Ptr7(:,:)

  END MODULE

  PROGRAM dataPtriPolyPtr2
  USE M 
  IMPLICIT NONE


  Ptr5(1:, 1:) => Ptr1
  IF (ANY( LBOUND(Ptr5) .NE. (/1,1 /))) STOP 11
  IF (ANY( UBOUND(Ptr5) .NE. (/1,10/))) STOP 12
  IF (ANY(Ptr5%I        .NE. -1))       STOP 13

  Ptr5(0:9, 0:0) => Ptr1(3, :)
  IF (ANY( LBOUND(Ptr5) .NE. (/0,0 /))) STOP 14
  IF (ANY( UBOUND(Ptr5) .NE. (/9,0 /))) STOP 15
  IF (ANY(Ptr5%I        .NE. -1   ))    STOP 16

  Ptr6(1:, 1:) => Ptr1
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) STOP 21
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) STOP 22
  Ptr7(1:, 1:) => Ptr6 
  IF (ANY(Ptr7%I      .NE. -1  ))      STOP 23

  Ptr6(0:8, 0:0) => Ptr1(3, :)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) STOP 24
  IF (ANY( UBOUND(Ptr6) .NE. (/8,0 /))) STOP 25
  Ptr7(1:, 1:) => Ptr6
  IF (ANY(Ptr7%I      .NE. -1   ))      STOP 26

  ALLOCATE(Ptr2(3:3, 10), SOURCE=DT(1))
  Ptr6(1:, 1:) => Ptr2
  IF (ANY( LBOUND(Ptr6) .NE. (/1,1 /))) STOP 31
  IF (ANY( UBOUND(Ptr6) .NE. (/1,10/))) STOP 32
  Ptr7(1:, 1:) => Ptr6 
  IF (ANY(Ptr7%I      .NE. 1    ))      STOP 33
  DEALLOCATE(Ptr2)

  ALLOCATE(Ptr2(30, 3:3), SOURCE=DT(3))
  Ptr6(0:8, 0:2) => Ptr2(:, 3)
  IF (ANY( LBOUND(Ptr6) .NE. (/0,0 /))) STOP 34
  IF (ANY( UBOUND(Ptr6) .NE. (/8,2 /))) STOP 35
  Ptr7(1:, 1:) => Ptr6
  IF (ANY(Ptr7%I      .NE. 3    ))      STOP 36
  DEALLOCATE(Ptr2)



  END


