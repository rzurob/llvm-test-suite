! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: AssocNameAsOther3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssocNameAsOther3
!*
!*  DATE                       : Feb. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The associate construct name is the same as globe entity's name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    LOGICAL(8) :: L0 = .TRUE._8
    LOGICAL(8), PRIVATE :: L1

    CONTAINS

    FUNCTION Fun(Arg)
    LOGICAL(8) :: Arg, Fun
      ASSOCIATE ( L1 => Arg )
        ASSOCIATE ( Fun => Fun )
          Fun = L1
        END ASSOCIATE
      END ASSOCIATE
    END FUNCTION

    SUBROUTINE Set(Arg0, Arg1)
    LOGICAL(8) :: Arg0, Arg1
      ASSOCIATE ( L1 => L0, L0 => L1  )
        L1 = Arg0
        L0 = Arg1
      END ASSOCIATE

    IF ( L0 .NEQV. Arg0 ) STOP 45
    IF ( L1 .NEQV. Arg1 ) STOP 45
    END SUBROUTINE

  END MODULE

  PROGRAM AssocNameAsOther3
  USE M

  ASSOCIATE ( L1 => Fun(L0) )
    IF ( .NOT. L1 ) STOP 11
  END ASSOCIATE

  ASSOCIATE ( L0 =>  .FALSE._1 )
    IF ( L0 ) STOP 12
  END ASSOCIATE

  IF ( .NOT. L0 ) STOP 13

  CALL Set ( .FALSE._8, .TRUE._8)
  IF (       L0 ) STOP 14

  END


