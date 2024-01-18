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
! %POSTCMD: tcomp PtrAssignTarExpr1.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PrtAssignTarExpr1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 12, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
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
!*  C726 (R742) An expr shall be a reference to a function whose result
!*  is a procedure pointer.
!* 
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS 

    FUNCTION IFun()
    INTEGER, POINTER :: IFun
      ALLOCATE(IFun, SOURCE=11)
    END FUNCTION

    FUNCTION RFun()
    REAL, POINTER :: RFun
      ALLOCATE(RFun, SOURCE=1.0)
    END FUNCTION

    FUNCTION CFun()
    CHARACTER(1), POINTER :: CFun
      ALLOCATE(CFun, SOURCE="X")
    END FUNCTION

    FUNCTION DFun()
    DOUBLE PRECISION, POINTER :: DFun
      ALLOCATE(DFun, SOURCE=1D0)
    END FUNCTION

    FUNCTION LFun()
    LOGICAL, POINTER :: LFun
      ALLOCATE(LFun, SOURCE=.TRUE.)
    END FUNCTION

    FUNCTION BFun()
    BYTE, POINTER :: BFun
      ALLOCATE(BFun, SOURCE=1_1)
    END FUNCTION

  END MODULE


  PROGRAM PrtAssignTarExpr1 
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION FunI()
      CLASS(*), POINTER :: FunI
    END FUNCTION
  END INTERFACE

  PROCEDURE(FunI), POINTER :: Ptr 

    Ptr  => IFun()

    Ptr  => RFun()

    Ptr  => CFun()

    Ptr  => DFun()

    Ptr  => LFun()

    Ptr  => BFun()

  END

