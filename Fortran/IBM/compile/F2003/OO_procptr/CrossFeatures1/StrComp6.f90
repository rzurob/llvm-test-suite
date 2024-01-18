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
! %POSTCMD: tcomp StrComp6.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp6.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 18, 2005
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
!*  A data-target shall correspond to a nonprocedure pointer component; 
!*  a proc-target shall correspond to a procedure pointer component
!*  (Err Msg wrong) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      PROCEDURE(), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      TYPE(Base)          :: BaseComp=BASE(NULL()) 
      TYPE(Base), POINTER :: BasePtr=>NULL()
      PROCEDURE()    , NOPASS, POINTER :: ProcPtr1=>NULL()
      PROCEDURE(TYPE(Base)), NOPASS, POINTER :: ProcPtr2=>NULL()
    END TYPE
   
    CONTAINS
 
    FUNCTION ModFun(Arg)
    TYPE(Base) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE
 
  PROGRAM StrComp6  
  USE M
  IMPLICIT NONE 

  TYPE(DT), TARGET  :: DTTar
  TYPE(DT), POINTER :: DTPtr
  PROCEDURE(TYPE(DT)),  POINTER :: ProcPtr=>NULL() 


  TYPE(DT) :: V

  V = DT(ModFun, NULL(), NULL(), NULL() )
  V = DT(Base(NULL()), ModFun, NULL(), NULL())
  V = DT(Base(NULL()), NULL(),DTTar, NULL() )
  V = DT(Base(NULL()), NULL(), NULL(), DTPtr)

  V = DT(Base(ModFun), NULL(), NULL(), NULL())

  END

