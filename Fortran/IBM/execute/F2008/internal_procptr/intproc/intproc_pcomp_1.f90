!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_pcomp_1.f
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
!*  Test procedure pointer component -- basic
!*    
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 
  PROGRAM intproc_pcomp_1
  INTEGER :: iarr(1000)
  
  TYPE :: DT(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)
    PROCEDURE(), POINTER, NOPASS :: procptr
  END TYPE

  TYPE(DT(:)), POINTER :: Ptr


  DO i = 1, 100
    ALLOCATE(DT(100):: Ptr)
    iarr = i
    Ptr%procptr => intsub1
    CALL intsub(ptr%procptr, i)
    IF ( ANY(iarr .NE. i+4) ) ERROR STOP 15
    DEALLOCATE(Ptr)
  END DO

  CONTAINS
    SUBROUTINE intsub(proc, iarg) 
    PROCEDURE() :: proc 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 11
    iarr = iarg + 1
    Ptr%procptr => intsub2 
    CALL proc(Ptr%procptr, iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub1(proc, iarg) 
    PROCEDURE() :: proc 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 12
    iarr = iarg+1 
    Ptr%procptr => intsub3 
    CALL proc(Ptr%procptr, iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub2(proc, iarg) 
    PROCEDURE() :: proc 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 13
    iarr = iarg + 1
    CALL proc(iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub3(iarg) 
    PROCEDURE() :: proc 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 14
    iarr = iarg + 1
    END SUBROUTINE

  END


