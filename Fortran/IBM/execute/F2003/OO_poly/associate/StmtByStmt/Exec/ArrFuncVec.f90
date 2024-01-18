! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrFuncVec.f  
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
!*  TEST CASE NAME             : ArrFuncVec
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 10, 2005
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
!*    The selector is a function return  with a vector subscript
!*    (Comp failed) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ArrFuncAllocSec
  IMPLICIT NONE
  INTEGER :: i
  LOGICAL(8) :: Arr(5,5)=.FALSE.
  INTEGER(1) :: S1(5)=(/5,4,3,2,1/), S2(5)=(/5,5,5,5,5/), S3(4)=(/1,2,3,4/)

  Arr(:,1:4) = .TRUE. 

  ASSOCIATE ( As => Fun(Arr(S1, S2)), As1 => Fun(Arr(S2, S2))  )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/5,5/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/5,5/) ) )             STOP 32
    IF ( ANY (SHAPE(As)   .NE. (/5,5/) ) )             STOP 32
    IF ( KIND(As)   .NE. 8 )                           STOP 33

    ASSOCIATE (As1 => Fun(As(S2, S2)))
      IF ( ANY (LBOUND(As)   .NE. (/1,1/) ) )             STOP 40
      IF ( ANY (UBOUND(As1)  .NE. (/5,5/) ) )             STOP 41
      IF ( ANY (SHAPE(As1)   .NE. (/5,5/) ) )             STOP 42
      IF ( ANY (SHAPE(As1)   .NE. (/5,5/) ) )             STOP 42
      IF ( KIND(As1)   .NE. 8 )                           STOP 43
    END ASSOCIATE

  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(Arg)
  LOGICAL(8) :: Arg(5,5)
  LOGICAL(8), ALLOCATABLE :: Fun(:,:)
    ALLOCATE(Fun(SIZE(Arg,1), SIZE(Arg,2)), SOURCE=Arg)
  END FUNCTION

  END
