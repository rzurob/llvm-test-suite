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
! %POSTCMD: tcomp VarDefContext2.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  VarDefContext2 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 28, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
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
!*   
!*  Diagnosis on varriable deinition context 
!* 
!*  (ICE-298990) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  VarDefContext2
  IMPLICIT NONE

  CONTAINS
  SUBROUTINE Sub(Arg)
   CLASS(*) ,INTENT(IN):: Arg

    SELECT TYPE (Ptr => Arg)
    CLASS DEFAULT
      STOP 40
    TYPE IS (CHARACTER(*))
      ASSOCIATE ( Ptr => Ptr)
        Ptr = "4321"
      END ASSOCIATE
    END SELECT
  END SUBROUTINE

  END

