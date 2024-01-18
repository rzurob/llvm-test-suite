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
! %POSTCMD: tcomp AssocNameSameNestedPtr.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  AssocNameSameNestedPtr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
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
!*   The associate name is the same as the selector within associate/select type
!*   constructs
!*   Test the pointer attribute
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  AssocNameSameNestedPtr
  IMPLICIT NONE

  TYPE :: Base
  END TYPE
 
  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar

  ASSOCIATE ( Ptr => Ptr )
  SELECT TYPE (  Ptr )
    TYPE IS (Base) 
      PRINT*, "OK!" 
      Ptr => Tar 
    CLASS IS (Base)
      STOP 20
    CLASS DEFAULT 
      STOP 30
  END SELECT 

  Ptr => Tar 

  SELECT TYPE ( Ptr )
    TYPE IS (Base) 
      PRINT*, "OK!" 
      Ptr => Tar 
    CLASS IS (Base)
      STOP 20
    CLASS DEFAULT 
      STOP 30
  END SELECT 
  END ASSOCIATE

  END

