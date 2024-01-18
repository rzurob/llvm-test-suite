!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_pcomp_2.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 28 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
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
!*
!*  Test procedure pointer component -- 
!*  structure constructor with procedure pointer component 
!*  associated with internal procedure 
!*    
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)
    PROCEDURE(), POINTER, NOPASS :: procptr
  END TYPE

  END MODULE
 
  PROGRAM intproc_pcomp_2
  USE M
  INTEGER :: iarr(1000)
  
  TYPE(DT(100)) :: T 


  DO i = 1, 100
    iarr = i
    T = DT(100)(i, intsub1) 
    IF ( .NOT. Associated(T%procptr, intsub1)) ERROR STOP 21
    CALL intsub(T, i)
    IF ( ANY(iarr .NE. i+4) ) ERROR STOP 15
  END DO

  CONTAINS
    SUBROUTINE intsub(T, iarg) 
    TYPE(DT(100)) :: T 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 11
    iarr = iarg + 1
    CALL T%procptr(DT(100)(1, intsub2), iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub1(T, iarg) 
    TYPE(DT(100)) :: T 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 12
    iarr = iarg+1 
    CALL T%procptr(DT(100)(1, intsub3), iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub2(T, iarg) 
    TYPE(DT(100)) :: T 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 13
    iarr = iarg + 1
    CALL T%procptr(T, iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub3(T, iarg) 
    TYPE(DT(100)) :: T 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 14
    iarr = iarg + 1
    END SUBROUTINE

  END


