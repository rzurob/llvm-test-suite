! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 22, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   The associating entity does not have the pointer attribute
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AttrPointer

  CONTAINS
  SUBROUTINE Sub(A)
  CLASS(*), POINTER :: A
  CLASS(*), POINTER :: B

    B => A
    ASSOCIATE ( A => A)
      B => A  ! This is correct
      PRINT*, ASSOCIATED(A, A)
      ALLOCATE( A )
      A => B
    END ASSOCIATE

  END SUBROUTINE
  END