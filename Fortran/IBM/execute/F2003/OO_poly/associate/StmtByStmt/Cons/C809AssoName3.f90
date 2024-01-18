! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  C809AssoName3.f  
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
!*  TEST CASE NAME             : C809AssoName3
!*  TEST CASE TITLE            : C809
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 20, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name 
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
!*     The associate name must only be declared once in the ASSOCIATE statement  
!*     Selector is an external function with the same name 
!*    (Ref 293110)
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM C809AssoName1

    ASSOCIATE ( As => As()  )
      IF (As .NE. 1.0) STOP 50

      ASSOCIATE ( As => As  )
        IF (As .NE. 1.0) STOP 51
      END ASSOCIATE

    END ASSOCIATE

  END 

  FUNCTION As()
    As = 1.0
  END FUNCTION

