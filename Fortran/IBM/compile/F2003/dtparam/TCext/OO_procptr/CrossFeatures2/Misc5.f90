! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/Misc5.f
! opt variations: -qnok -ql

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
! %POSTCMD: tcomp Misc5.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 29, 2005
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
!*  Procedure pointer - Diag on the pass attr
!*  (update vf when 312629 done)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: DT(K1)    ! (4)
        INTEGER, KIND :: K1
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    END TYPE

    INTERFACE
      FUNCTION IFun(Arg)
      IMPORT DT
        CLASS(DT(4)) :: Arg(1)
        TYPE(DT(4))  :: IFun
      END FUNCTION
    END INTERFACE
  END MODULE

  MODULE M1
    TYPE :: DT1(K2)    ! (4)
        INTEGER, KIND :: K2
      PROCEDURE(IFun1), PASS, POINTER :: ProcPtr
    END TYPE
    INTERFACE 
      FUNCTION IFun1(Arg)
      IMPORT
        CLASS(DT1(4)):: Arg(:)
        TYPE(DT1(4)) :: IFun1(SIZE(Arg))
      END FUNCTION
    END INTERFACE
  END MODULE

  MODULE M2
    TYPE :: DT2(K3)    ! (4)
        INTEGER, KIND :: K3
      PROCEDURE(IFun2), PASS, POINTER :: ProcPtr
    END TYPE
    INTERFACE  
      FUNCTION IFun2(Arg)
      IMPORT
        TYPE(DT2(4)) :: Arg
        TYPE(DT2(4)) :: IFun2
      END FUNCTION
    END INTERFACE
  END MODULE

  MODULE M3
    TYPE :: DT2(K4)    ! (4)
        INTEGER, KIND :: K4
    END TYPE

    TYPE :: DT3(K5)    ! (4)
        INTEGER, KIND :: K5
      SEQUENCE
      PROCEDURE(IFun3), PASS, POINTER :: ProcPtr
    END TYPE
    INTERFACE 
      FUNCTION IFun3(Arg)
        IMPORT 
        CLASS(DT2(4)) :: Arg
        TYPE(DT2(4)) :: IFun3
      END FUNCTION
    END INTERFACE
  END MODULE

  PROGRAM Misc5 
  END

