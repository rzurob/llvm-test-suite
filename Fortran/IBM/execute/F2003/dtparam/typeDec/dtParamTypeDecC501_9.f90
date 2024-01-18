!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_9
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 02, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
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
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is 
!*  not a colon or an asterisk shall be a specification-expr
!*
!*  -- A type parameter of the derived type being defined 
!*   
!*   (336400)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    CHARACTER(LEN=L)  :: C
    TYPE(DT(K, L=L)),   POINTER  :: Ptr1 
    TYPE(DT(K, L=L+1)), POINTER  :: Ptr2 => NULL()
    TYPE(DT(K, L=L+2)), POINTER  :: Ptr3(:) => NULL()
  END TYPE

  CONTAINS

  PURE FUNCTION ModFun(Arg)
  INTEGER :: ModFun
  INTEGER, INTENT(IN) :: Arg
    ModFun = Arg
  END FUNCTION

  SUBROUTINE ModSub(L)
  INTEGER :: L
  TYPE(DT(K=4, L=L)) :: T  
  TYPE(DT(K=4, L=ModFun(L+3))), POINTER :: Ptr4(:) 

  allocate (t%ptr1, t%ptr2, t%ptr3(1))

  IF ( T%K      .NE.  4  )  STOP 11
  IF ( T%L      .NE.  2  )  STOP 12
  IF ( T%C%LEN  .NE.  2  )  STOP 13

  IF ( T%Ptr1%K .NE.  4  )  STOP 21
  IF ( T%Ptr1%L .NE.  2  )  STOP 22

  IF ( T%Ptr2%K .NE.  4  )  STOP 31
  IF ( T%Ptr2%L .NE.  3  )  STOP 32

  IF ( T%Ptr3%K .NE.  4  )  STOP 41
  IF ( T%Ptr3%L .NE.  4  )  STOP 42

  IF ( Ptr4%K   .NE.  4  )  STOP 51
  IF ( Ptr4%L   .NE.  5  )  STOP 52


  END SUBROUTINE

  END MODULE

  
  PROGRAM dtParamTypeDecC501_9
  USE M

  CALL ModSub( 2 )

  END

