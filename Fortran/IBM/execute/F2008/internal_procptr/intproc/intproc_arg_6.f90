!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_6.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 25 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Test the argument association --
!*    The internal function result is DTP with auto type parameter.
!*   
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: DT(l1, l2)
      INTEGER, LEN :: L1
      INTEGER, LEN :: L2
      INTEGER :: Arr1(L1,L2)
      INTEGER :: Arr2(L2,L1) 
    END TYPE
    TYPE(DT(:,:)), ALLOCATABLE :: R
  CONTAINS

  SUBROUTINE Intfunccall(proc, n,m,arg)
  procedure(Intfunc) :: proc
  TYPE(DT(n,m)) ::  arg
   R = proc(n,m,arg) 
  END SUBROUTINE 

  FUNCTION Intfunc(n,m, arg)
  TYPE(DT(n,m)) :: Intfunc, arg 
    Intfunc = arg 
  END FUNCTION 

  SUBROUTINE Intcheck(i,j)
    DO ii=1, i
    DO jj=1, j
      IF ( R%Arr1(ii,jj) .NE. ii*jj) ERROR STOP 11 
      IF ( R%Arr2(jj,ii) .NE. ii+jj) ERROR STOP 12 
    END DO
    END DO
  END SUBROUTINE


  END MODULE
 
  PROGRAM intproc_arg_6
  USE M
  TYPE(DT(:,:)), ALLOCATABLE :: RR

  DO I=1, 20
  DO J=1, 30 
    ALLOCATE(DT(I,J)::RR)
    CALL callsub(Intset, i, j)
    CALL Intfunccall(Intfunc, i, j, RR)  
    CALL callsub(Intcheck, I, J) 
    DEALLOCATE(RR)
  END DO
  END DO

  CONTAINS
  SUBROUTINE callsub(proc,i,j) 
   PROCEDURE() :: proc
   CALL proc(i,j)
  END SUBROUTINE

  SUBROUTINE Intset(i,j)
    DO ii=1, i
    DO jj=1, j
      RR%Arr1(ii,jj) = ii*jj 
      RR%Arr2(jj,ii) = ii+jj 
    END DO
    END DO
  END SUBROUTINE

  END

