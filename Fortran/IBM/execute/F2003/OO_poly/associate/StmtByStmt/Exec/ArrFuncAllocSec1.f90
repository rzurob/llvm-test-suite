! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrFuncAllocSec1.f  
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
!*  TEST CASE NAME             : ArrFuncAllocSec1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
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
!*    The selector is a function return a section of allocatable array 
!*    (ICE-305139) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ArrFuncAllocSec1
  IMPLICIT CHARACTER(129)(C) 
  INTEGER :: i

 
  ASSOCIATE ( As => Fun(RESHAPE((/C1, C2, C3, C4/), (/2,2/))) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32

    IF ( TRIM(As(1,1)) .NE. "1" ) STOP 33 
    IF ( TRIM(As(2,1)) .NE. "2" ) STOP 34 
    IF ( TRIM(As(1,2)) .NE. "3" ) STOP 35 
    IF ( TRIM(As(2,2)) .NE. "4" ) STOP 35 
    IF (  LEN(As)      .NE. 129 ) STOP 36 

    ASSOCIATE ( As => FUN(As) )

      IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 40
      IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 41
      IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 42

      IF ( TRIM(As(1,1)) .NE. "1" ) STOP 43
      IF ( TRIM(As(2,1)) .NE. "2" ) STOP 44
      IF ( TRIM(As(1,2)) .NE. "3" ) STOP 45
      IF ( TRIM(As(2,2)) .NE. "4" ) STOP 45
      IF ( LEN(As)       .NE. 129 ) STOP 46

    END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(Arg)
  CHARACTER(*) :: Arg(:,:)
  CHARACTER(LEN(Arg)), ALLOCATABLE :: Fun(:,:)
    ALLOCATE(Fun(SIZE(Arg,1), SIZE(Arg,2)), SOURCE=Arg)
    Fun = RESHAPE((/"1","2","3","4"/), (/2,2/))
  END FUNCTION

  END

