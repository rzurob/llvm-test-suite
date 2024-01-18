! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncULPolyAlloc.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  FuncULPolyAlloc.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncULPolyAlloc
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a function call returning an unlimited poly allocatable 
!*   
!*    (299787) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseID=1 
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      TYPE(Base(K1)) :: BS=Base(K1)(0) 
      CLASS(Base(K1)), POINTER :: BSPtr=>NULL()
      INTEGER(K1) :: ChildID=2 
    END TYPE

  CONTAINS
   
    FUNCTION ReturnObj(Arg)
    TYPE (Child(4)) :: Arg
    CLASS (Base(4)), ALLOCATABLE  :: ReturnObj
      ALLOCATE(ReturnObj, SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM FuncULPolyAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child(4)), TARGET :: V=Child(4)(BaseID=-1, ChildID=-2) 

  ASSOCIATE ( As => TRANSFER(ReturnObj(V), ReturnObj(V)) )

    SELECT TYPE (As)
    TYPE IS (Child(4))

      IF ( As%ChildID    .NE. -2  )         STOP 33
      IF ( As%BS%BaseID  .NE.  0  )         STOP 34
      IF ( As%BaseID     .NE. -1  )         STOP 35

      ASSOCIATE (As => As)
        IF ( ASSOCIATED(As%BSPtr) )         STOP 36
      END ASSOCIATE

    CLASS DEFAULT
      STOP 88 
    END SELECT

  END ASSOCIATE

  END

