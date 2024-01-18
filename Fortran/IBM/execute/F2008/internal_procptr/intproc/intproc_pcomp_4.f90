!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_pcomp_4.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 29 2011
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
!*    The type of variable is polymorphic.
!*   
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: Base(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)
  END TYPE

  TYPE, EXTENDS(Base) :: DT
    PROCEDURE(INTSUB), POINTER, NOPASS :: procptr
  END TYPE
  INTEGER :: iarr(1000)

  CONTAINS
    SUBROUTINE intsub(T, iarg) 
    CLASS(Base(*)) :: T 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 11
    SELECT TYPE (TT => T)
    TYPE IS (DT(*))
      IF ( .NOT. Associated(TT%procptr, intsub1)) ERROR STOP 22
      iarr = iarg + 1
      CALL TT%procptr(T, iarg+1)
    CLASS DEFAULT
      ERROR STOP 33 
    END SELECT 

    END SUBROUTINE

    SUBROUTINE intsub1(T, iarg) 
    CLASS(Base(*)) :: T 

    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 12
    SELECT TYPE (TT => T)
    TYPE IS (DT(*))
      !IF ( .NOT. Associated(TT%procptr, intsub1)) ERROR STOP 23 !388927 was considered illegal 
    CLASS DEFAULT
      ERROR STOP 34 
    END SELECT 
    iarr = iarg+1 
    END SUBROUTINE

  END MODULE
 
  PROGRAM intproc_pcomp_4
  USE M
  
  CLASS(Base(:)), ALLOCATABLE :: T 

  DO i = 1, 100
    iarr = i
    ALLOCATE(T, SOURCE=DT(i)(i, intsub1))

    SELECT TYPE (TT => T)
    TYPE IS (DT(*))
      IF ( .NOT. Associated(TT%procptr, intsub1)) ERROR STOP 21
    CLASS DEFAULT
      ERROR STOP 35 
    END SELECT 

    CALL intsub(T, i)
    DEALLOCATE(T)
    IF ( ANY(iarr .NE. i+2) ) ERROR STOP 15
  END DO

  END


