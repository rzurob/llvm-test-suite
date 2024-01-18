! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp AttrPointer.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrPointer
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 22, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
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
