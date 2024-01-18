! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  IntrinChar.f  
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
!*  TEST CASE NAME             : IntrinChar
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 07, 2005
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
!*    The selector is a character literal of default 
!*    (ICE-300823) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IntrinChar
  IMPLICIT NONE
  INTEGER :: K
  PARAMETER (K=1)

  ASSOCIATE ( As => 1_"1234567890" )
    IF ( As  .NE. 1_"1234567890" )        STOP 20 
    ASSOCIATE ( As => As )
      IF ( As       .NE. 1_"1234567890" ) STOP 21 
      IF ( KIND(As) .NE. 1 )              STOP 22 
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => K_"1234567890" )
    IF ( As    .NE. K_"1234567890" )      STOP 30 
    ASSOCIATE ( As => As )
      IF ( As       .NE. K_"1234567890" ) STOP 31 
      IF ( KIND(As) .NE. 1 )              STOP 32 
    END ASSOCIATE
    CALL Sub(As)
  END ASSOCIATE


  CONTAINS
  
  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg
  
  SELECT TYPE(Arg)
  TYPE IS (CHARACTER(*))
    ASSOCIATE ( Arg=>Arg)
      IF ( Arg       .NE. K_"1234567890" ) STOP 41
      IF ( KIND(Arg) .NE. 1 )              STOP 42
    END ASSOCIATE
  CLASS DEFAULT
    STOP 44
  END SELECT

  END SUBROUTINE

  END

