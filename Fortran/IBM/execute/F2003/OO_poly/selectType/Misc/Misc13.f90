! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Misc13.f
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
!*  TEST CASE NAME             : Misc11 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 05, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : 
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
!*  
!*  Associating entity's properties are not correct. 
!*    (297764)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc13

  CLASS(*), POINTER :: Arr(:)

  ALLOCATE(INTEGER :: Arr(1111))

  SELECT TYPE ( As => Arr(2:999:2))
  CLASS DEFAULT
    print*, SIZE(As)
    print*, ubound(as)
    print*, lbound(as)
    IF ( SIZE(As)      .NE. 499 ) STOP 20
    IF ( UBOUND(As, 1) .NE. 499 ) STOP 22
    IF ( LBOUND(As, 1) .NE. 1 )   STOP 24
  END SELECT

  END


