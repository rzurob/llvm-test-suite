!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC717.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 2, 2006
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
!*  C717 (R735) If data-target is unlimited polymorphic, data-pointer-object shall be
!*  unlimited polymorphic,  of a sequence derived type, or of a type with 
!*  the BIND attribute.  
!*   
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC717 
  IMPLICIT NONE

  INTEGER, POINTER :: ptr(:)
 
  TYPE :: DT
  END TYPE

  TYPE(DT ), POINTER :: Ptr1(:)
  CLASS(DT ), POINTER  :: Ptr2(:)

  CLASS(*), POINTER :: T(:) 

  CLASS(*), POINTER :: Ptr3(:)
 
  TYPE :: DT1
    SEQUENCE
  END TYPE

  TYPE(DT1), POINTER :: Ptr4(:)

  TYPE, BIND(C) :: DT2
   
  END TYPE

  TYPE(DT1), POINTER :: Ptr5(:,:)


  Ptr(1:3) => T 

  Ptr1(-1:) => T
 
  Ptr2(3:0) => T 

  ! the following shall be ok
  Ptr3(3:) => T   
  Ptr4(1:3) => T   
  Ptr5(1:1, 1:3) => T   

  END


