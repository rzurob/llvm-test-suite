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
! %POSTCMD: tcomp Misc25.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc24
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 16, 2005
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
!*  
!*  (300157)
!* 
!*  
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc25


  TYPE :: DT
   INTEGER :: i=1
  END TYPE

  TYPE(DT) :: A(10)

  ASSOCIATE ( As => A(::2) )
  ASSOCIATE ( As => as(::2) )
    IF ( ANY (LBOUND(As)      .NE. (/1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)      .NE. (/3/) ) )             STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/3/) ) )             STOP 32
    IF ( ANY (As%i      .NE. (/1,1,1,1/) ) ) STOP 33  !<- no complain
    IF ( ANY (As%i      .NE. (/1/) ) )       STOP 34  !<- no complain
  !  IF ( ANY ((/1,1,1/)      .NE. (/1/) ) ) STOP 34
    IF ( ANY (As%i      .NE. (/1,1,1/) ) ) STOP 35
  END ASSOCIATE
  END ASSOCIATE
  END



