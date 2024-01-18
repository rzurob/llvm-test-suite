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
! %POSTCMD: tcomp VarDef10.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef10
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 22, 2005
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
!*   Variable Definition Context on non variable selector 
!*   - ALLOCATE:IOSTAT, IOMSG
!* 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef10
  IMPLICIT NONE
  INTEGER :: J(2)
  INTEGER, POINTER :: Obj(:)

  ASSOCIATE ( STAT => J((/1/)), ERRMSG => "ok" )
    ALLOCATE(Obj(3), STAT=STAT(1)) 
    DEALLOCATE(Obj, ERRMSG=ERRMSG) 
  END ASSOCIATE

  END 

