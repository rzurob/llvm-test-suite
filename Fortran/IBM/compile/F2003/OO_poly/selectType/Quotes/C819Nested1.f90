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
! %POSTCMD: tcomp C819Nested1.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  C819Nested1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C819 
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
!*    The same construct name is reused in nested constructs 
!*    
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  C819Nested1
  IMPLICIT NONE

  TYPE :: Base
  END TYPE
 
  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar

99 SELECT : ASSOCIATE ( As=>Ptr)
    SELECT :  SELECT TYPE ( As )
1   TYPE IS (Base) SELECT  
      goto 11 
11    PRINT*, "OK!"  
2   CLASS IS (Base)
22    STOP 20
3   CLASS DEFAULT SELECT
4      SELECT TYPE (Ptr)
        CLASS IS (Base) 
         STOP 20
5     END SELECT 
33    STOP 30
333 END SELECT  SELECT
   END ASSOCIATE SELECT


  END

