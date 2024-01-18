!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExt.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 07, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
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
!*  If no bounds-remapping-list is specified, the extent of a dimension of data-pointer-object
!*  is the extent of the corresponding dimension of data-target.
!*  
!*
!*  
!*  (323080)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0
    CLASS(DT0), POINTER :: Ptr(:,:)
  END TYPE
  
  TYPE, EXTENDS(DT0) :: DT
  END TYPE

  TYPE (DT), TARGET  :: T(1000,2)

  END MODULE 

  PROGRAM dataPtrExt 
  USE M
  IMPLICIT NONE


  CALL ModSub(T(1, 1))

  CONTAINS

  SUBROUTINE ModSub(Arg)
  TYPE(DT), TARGET   :: Arg(1000,2)
  INTEGER            :: I

    DO I=1 ,1000
      Arg(I, 1)%Ptr(-999:, -1:) => Arg 
      IF (ANY( LBOUND( Arg(I, 1)%Ptr) .NE. (/-999, -1/))) STOP 11 
      IF (ANY( SHAPE(  Arg(I, 1)%Ptr) .NE. (/1000,  2/))) STOP 12 
      Arg(I, 2)%Ptr(-499:500, -1:-1) => Arg(:, 2)
      IF (ANY( LBOUND( Arg(I, 2)%Ptr) .NE. (/-499, -1/))) STOP 21 
      IF (ANY( SHAPE(  Arg(I, 2)%Ptr) .NE. (/1000,  1/))) STOP 22 
    END DO
     
  END SUBROUTINE


  END


