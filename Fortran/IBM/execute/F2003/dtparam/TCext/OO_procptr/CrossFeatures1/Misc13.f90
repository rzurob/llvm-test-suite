! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Misc13.f
! opt variations: -ql

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
! %POSTCMD: tcomp Misc13.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc13.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 09, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
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
!* Volatile - no external, no intent 
!*  
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc13

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: I=1
  END TYPE

  PROCEDURE(), VOLATILE                :: ProcPtr1
  PROCEDURE(IntFun), VOLATILE          :: ProcPtr2 
  PROCEDURE(IntFun), VOLATILE, POINTER :: ProcPtr3


  CONTAINS

  SUBROUTINE IntSub(Arg)
  PROCEDURE(IntFun), VOLATILE :: arg
  END SUBROUTINE

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IntFun), VOLATILE, INTENT(IN), POINTER :: Arg
  !PROCEDURE(IntFun),  INTENT(IN), POINTER :: Arg
  END SUBROUTINE

  FUNCTION IntFun(Arg)
  TYPE(DT(4)) :: IntFun, Arg
    IntFun = Arg
  END FUNCTION

  END


