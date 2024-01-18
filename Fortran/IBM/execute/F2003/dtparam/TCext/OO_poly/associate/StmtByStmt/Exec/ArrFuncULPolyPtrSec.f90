! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncULPolyPtrSec.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrFuncULPolyPtrSec.f  
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
!*  TEST CASE NAME             : ArrFuncULPolyPtrSec
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
!*    The selector is a function call returning an unlimited poly array 
!*    test the section of the selector 
!*    (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseID=1 
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      TYPE(Base(K1)) :: BS 
      CLASS(Base(K1)), POINTER :: BSPtr
      INTEGER(K1) :: ChildID=2 
    END TYPE

  CONTAINS
   
    FUNCTION ReturnArr(Arg)
    TYPE (Child(4)) :: Arg(:)
    CLASS (*), POINTER :: ReturnArr(:)
      ALLOCATE(ReturnArr(SIZE(Arg)), SOURCE=Arg(:))
    END FUNCTION

  END MODULE

  PROGRAM ArrFuncULPolyPtrSec
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child(4)) :: Arr(5)

  Arr =  (/(Child(4)(Base=Base(4)(BaseID=0),      &
  &                  BS=Base(4)(BaseID=-1),    &
  &                  BSPtr=NULL(),          &
  &                  ChildID=-2)            &
  &            , i=1,5)/) 

  ASSOCIATE ( As => ReturnArr(TRANSFER(Arr,Child(4)(BS=Base(4)(),BSPtr=NULL()),SIZE(Arr))) )

    IF ( ANY (LBOUND(As)  .NE. (/1/) ) )   STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/5/) ) )   STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/5/) ) )   STOP 32
  
    SELECT TYPE (As => As)
    TYPE IS (Child(4))
    
      IF ( ANY (As%ChildID    .NE. -2 ))         STOP 33
      IF ( ANY (As%BS%BaseID  .NE. -1 ))         STOP 34
      IF ( ANY (As%BaseID     .NE.  0 ))         STOP 35

      DO i =1, SIZE(As)

        ASSOCIATE (As => As)
        END ASSOCIATE

        IF ( ASSOCIATED(As(i)%BSPtr) )       STOP 36
 
        ASSOCIATE (As => As(i)%BS )
          IF ( As%BaseID  .NE. -1 )           STOP 37
        END ASSOCIATE 

      END DO
   

    CLASS DEFAULT
      STOP 88 
    END SELECT

  END ASSOCIATE

  END

