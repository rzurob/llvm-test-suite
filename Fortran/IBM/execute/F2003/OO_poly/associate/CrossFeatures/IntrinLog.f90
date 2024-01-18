! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  IntrinLog.f  
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
!*  TEST CASE NAME             : IntrinLog
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
!*    The selector is of logical type with various kinds
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IntrinLog
  IMPLICIT NONE

  ASSOCIATE ( As => .TRUE._1 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As    .NEQV.  .TRUE._1 ) STOP 20 
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => .TRUE._2 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As  .NEQV.  .TRUE._2 ) STOP 22 
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => .TRUE._4 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As    .NEQV. .TRUE._4 ) STOP 24 
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => .TRUE._8 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As    .NEQV. .TRUE._8 ) STOP 28 
    END ASSOCIATE
  END ASSOCIATE

  CONTAINS
  
  FUNCTION Fun(Arg)
  CLASS(*) :: Arg
  LOGICAL(1), POINTER  :: Fun

    SELECT TYPE (Arg)
    CLASS DEFAULT
      STOP 99
    TYPE IS (LOGICAL(1))
      ASSOCIATE ( Arg => Arg)
        ALLOCATE (Fun, SOURCE=LOGICAL(Arg, 1)) 
      END ASSOCIATE
    TYPE IS (LOGICAL(2))
      ASSOCIATE ( Arg => Arg)
        ALLOCATE (Fun, SOURCE=LOGICAL(Arg, 1)) 
      END ASSOCIATE
    TYPE IS (LOGICAL(4))
      ASSOCIATE ( Arg => Arg)
        ALLOCATE (Fun, SOURCE=LOGICAL(Arg, 1)) 
      END ASSOCIATE
    TYPE IS (LOGICAL(8))
      ASSOCIATE ( Arg => Arg)
        ALLOCATE (Fun, SOURCE=LOGICAL(Arg, 1)) 
      END ASSOCIATE
    END SELECT

  END FUNCTION

  END

