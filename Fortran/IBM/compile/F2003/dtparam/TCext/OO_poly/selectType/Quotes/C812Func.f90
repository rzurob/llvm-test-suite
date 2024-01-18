! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/C812Func.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C812Func.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C812Func
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 3, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812 
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
!*    The selector is a function call, associate name appears in variablr
!*    definition context
!*    
!*    (Wrong Msg: "The selector in the SELECT TYPE statement is not a named variable.
!*     An associate name should appear" )
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C812Func
  IMPLICIT NONE

  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  TYPE, EXTENDS(Base) :: Child    ! (4,20)
    TYPE(Base(K1,N1)) :: BaseArr(1)
  END TYPE
 
  SELECT TYPE ( As => Fun(Base(4,20)()) )
    TYPE IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      STOP 30
    CLASS IS (Child(4,*))
      As = Child(4,20)( BaseArr=(/Base(4,20)()/) ) 
  END SELECT 
  STOP 40

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Base(4,*)) :: Arg
  CLASS(Base(4,:)), POINTER :: Fun
    ALLOCATE( Fun, SOURCE=Child(4,20)(BaseArr=(/Base(4,20)()/) ) )
    SELECT TYPE( Fun )
      TYPE IS (Child(4,*))
        Fun%Base=Arg
      CLASS DEFAULT 
        STOP 22
    END SELECT
  END FUNCTION

  END

