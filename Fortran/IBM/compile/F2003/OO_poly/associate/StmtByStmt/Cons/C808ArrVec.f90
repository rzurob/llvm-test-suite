! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C808ArrVec.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C808ArrVec
!*  TEST CASE TITLE            : C808
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 20, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Selector is a constant
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
!*    The selector is an array with vector subscript 
!*    (ICE)   
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM C808ArrVec
  IMPLICIT NONE
  CHARACTER :: Arr(10) 
  INTEGER   :: i,Script(3) = (/1, 3, 7/)

    ASSOCIATE ( As => Arr((/1/)))
      As = "2"
      PRINT*, As 
    END ASSOCIATE

    ASSOCIATE ( As => Arr(Script) )
      As = As  
    END ASSOCIATE

    ASSOCIATE ( As => Arr( (/(i, i=1, 10, 2)/)) )
      As = "!" 
    END ASSOCIATE


  END
