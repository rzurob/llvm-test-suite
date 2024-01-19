! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 29, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
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
    TYPE :: DT
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    END TYPE

    INTERFACE
      FUNCTION IFun(Arg)
      IMPORT DT
        CLASS(DT) :: Arg(1)
        TYPE(DT)  :: IFun
      END FUNCTION
    END INTERFACE
  END MODULE

  MODULE M1
    TYPE :: DT1
      PROCEDURE(IFun1), PASS, POINTER :: ProcPtr
    END TYPE
    INTERFACE
      FUNCTION IFun1(Arg)
      IMPORT
        CLASS(DT1):: Arg(:)
        TYPE(DT1) :: IFun1(SIZE(Arg))
      END FUNCTION
    END INTERFACE
  END MODULE

  MODULE M2
    TYPE :: DT2
      PROCEDURE(IFun2), PASS, POINTER :: ProcPtr
    END TYPE
    INTERFACE
      FUNCTION IFun2(Arg)
      IMPORT
        TYPE(DT2) :: Arg
        TYPE(DT2) :: IFun2
      END FUNCTION
    END INTERFACE
  END MODULE

  MODULE M3
    TYPE :: DT2
    END TYPE

    TYPE :: DT3
      SEQUENCE
      PROCEDURE(IFun3), PASS, POINTER :: ProcPtr
    END TYPE
    INTERFACE
      FUNCTION IFun3(Arg)
        IMPORT
        CLASS(DT2) :: Arg
        TYPE(DT2) :: IFun3
      END FUNCTION
    END INTERFACE
  END MODULE

  PROGRAM Misc5
  END

