! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  HostAssocConstExp2.f  
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
!*  TEST CASE NAME             : HostAssocConstExp2 
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
!*    The selector is an associte name associating to a constant expression 
!*    with user defined operator 
!*    (ICE)
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM HostAssocConstExp2
  IMPLICIT NONE
  
  INTERFACE OPERATOR ( .PLUS. )
    FUNCTION Array_Add (Arg1, Arg2)
      INTEGER, INTENT (IN) :: Arg1 (:), Arg2(:)
      INTEGER    :: Array_Add (MIN(SIZE(Arg1), SIZE(Arg2)))
    END FUNCTION 
  END INTERFACE OPERATOR ( .PLUS. )

  INTEGER :: i
  INTEGER :: Int1(10) = 1 
 
    ASSOCIATE ( T0 => Int1 .PLUS. (/( i, i = 1, 15) /) )
    ASSOCIATE ( As0 => T0)
    ASSOCIATE ( As1 => As0(1:10:2))
      IF ( Any( As0 .NE. (/( i+1, i = 1, 10) /)) )    STOP 40
      IF ( Any( As1 .NE. (/( i+1, i = 1, 10, 2) /)) ) STOP 41
    END ASSOCIATE
    END ASSOCIATE
    END ASSOCIATE

  END

  FUNCTION Array_Add (Arg1, Arg2)
    INTEGER, INTENT (IN) :: Arg1 (:), Arg2(:)
    INTEGER    :: Array_Add (MIN(SIZE(Arg1), SIZE(Arg2)))

    DO i=1, MIN(SIZE(Arg1), SIZE(Arg2))
      Array_Add(i) = Arg1(i) + Arg2(i)
    END DO
  END FUNCTION 

