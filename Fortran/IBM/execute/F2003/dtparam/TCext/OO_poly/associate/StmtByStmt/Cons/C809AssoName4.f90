! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C809AssoName4.f
! with manual adjustment (line 83)
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  C809AssoName4.f  
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
!*  TEST CASE NAME             : C809AssoName4
!*  TEST CASE TITLE            : C809
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 20, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name 
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
!*     The associate name must only be declared once in the ASSOCIATE statement  
!*     Selector is structure constructor with the same name as associate name 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 
  MODULE M

    TYPE, ABSTRACT :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20) 
      INTEGER(K1)  :: Id = 0
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild 
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

    FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM C809AssoName4
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)) :: V = Child(4,20)(-1) 
 
    ASSOCIATE ( Child => Child(4,20)(Id=1)  )
      IF (Child%Id      .NE. 1) STOP 50
      IF (Child%GetId() .NE. 1) STOP 51
 
      ASSOCIATE ( Child => Child  )
        IF (Child%Id      .NE. 1) STOP 52
        IF (Child%GetId() .NE. 1) STOP 53
      END ASSOCIATE
    END ASSOCIATE

    ASSOCIATE ( V => V  )
      IF (V%Id      .NE. -1) STOP 60
      IF (V%GetId() .NE. -1) STOP 61

      V%Id = -2
 
      ASSOCIATE ( V => V  )
        IF (V%Id      .NE. -2) STOP 62
        IF (V%GetId() .NE. -2) STOP 63
      END ASSOCIATE
    END ASSOCIATE


  END

