! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Data2.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Data2.f 
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
!*  TEST CASE NAME             : Data2.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 12, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
!*
!*  REFERENCE                  : Feature 289058 
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
!*  An array name, array section, or array element that appears in a DATA 
!*  statement shall have had its array properties established by
!*  its array properties established by a previous specification statement.  
!*  (305762) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Data2 
  IMPLICIT TYPE(DT(20,4))(V) 

    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(), NOPASS, POINTER :: ProcPtr
    END TYPE

  DIMENSION V(3) 
  DATA V /3*DT(20,4)(-1, NULL())/ !fine 
  TYPE (DT(20,4)) :: V

  DIMENSION V1(3) 
  DATA V1(2)%ProcPtr /NULL()/  
! TYPE (DT) :: V1

  DATA V2%ProcPtr /NULL()/  
! TYPE (DT) :: V2

  DATA V3%ProcPtr /NULL()/  

  END


