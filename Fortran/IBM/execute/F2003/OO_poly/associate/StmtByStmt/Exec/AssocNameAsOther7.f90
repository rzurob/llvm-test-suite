! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: AssocNameAsOther7.f 
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
!*  TEST CASE NAME             : AssocNameAsOther7 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 01, 2005
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
!*    The associate construct name is the same as a common blk name 
!*   () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 

  PROGRAM AssocNameAsOther7
  INTEGER :: i, Arr(2:9)
  COMMON /cbk/i, Arr 

  ASSOCIATE ( Cbk => Arr)
    IF ( ANY(LBOUND(cbk) .NE. (/2/) )) STOP 11 
    IF ( ANY(SHAPE(cbk)  .NE. (/8/) )) STOP 12 
    IF ( ANY(cbk    .NE. -1) )         STOP 13 

    ASSOCIATE ( Init => Cbk)
      IF ( ANY(LBOUND(Init) .NE. (/2/) )) STOP 11 
      IF ( ANY(SHAPE(Init)  .NE. (/8/) )) STOP 12 
      IF ( ANY(Init    .NE. -1) )         STOP 13 
 
      Init = 1
      IF ( ANY(Arr    .NE. 1) )         STOP 31 
      IF ( ANY(Init   .NE. 1) )         STOP 32 
      IF ( ANY(Cbk    .NE. 1) )         STOP 33 

    END ASSOCIATE
  END ASSOCIATE 
  IF ( ANY(Arr    .NE. 1) )         STOP 14 

  END

  BLOCK DATA INIT

  INTEGER :: i, Arr(2:9)
  COMMON /cbk/i, Arr
  DATA Arr /8*-1/
  END BLOCK DATA
