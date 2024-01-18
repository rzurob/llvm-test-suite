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
! %POSTCMD: tcomp Misc28.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc28 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 09, 2005
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
!*   Diagnosis on elemental subroutine 
!*    (comp pass-300954) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 
  TYPE :: DT
    INTEGER :: Id = 1
  END TYPE

  CLASS(*), POINTER :: T

  CONTAINS

    ELEMENTAL SUBROUTINE Set(Arg)
    CLASS(*), INTENT(IN) :: Arg
    ASSOCIATE( AS => Arg)
      ALLOCATE(T, SOURCE=Arg)
    END ASSOCIATE

  END SUBROUTINE

  END


