! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : diagC448.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-11-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS Attribute   
!*                             :
!*  SECONDARY FUNCTIONS TESTED : - 
!*                               - 
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - 
!*                               - 
!*  C448:
!*       If the CONTIGUOUS attribute is specifed, the component shall be an array 
!*       with the POINTER attribute.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1), POINTER, CONTIGUOUS ::  cptr(:)
      END TYPE Base

      TYPE Bad1(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1), ALLOCATABLE, CONTIGUOUS ::  all(:)
      END TYPE Bad1

      TYPE Bad2(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1), CONTIGUOUS ::  arr(l1)
      END TYPE Bad2

      TYPE Bad3(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1), CONTIGUOUS ::  cmp
      END TYPE Bad3

      TYPE, EXTENDS(Base) :: List
        TYPE(List(k1,l1)), POINTER, CONTIGUOUS :: Next(:) => NULL()
        CONTAINS
        PROCEDURE, PRIVATE :: Dfoo
        PROCEDURE, PRIVATE :: Sfoo
        GENERIC :: foo => Sfoo , Dfoo
      END TYPE List

      INTEGER, PARAMETER :: single = KIND(0.0), double = KIND(0d0), len1 = 10

      CONTAINS

      TYPE(List(single,len1)) FUNCTION Sfoo (Obj) result (answer)
        CLASS(List(single,*)), TARGET  :: OBJ
        POINTER    :: answer(:)
        CONTIGUOUS :: answer

           answer => OBJ%NEXT
      END FUNCTION Sfoo

      TYPE(List(double,len1)) FUNCTION Dfoo (Obj) result (answer)
        CLASS(List(double,*)), TARGET  :: OBJ
        POINTER    :: answer(:)
        CONTIGUOUS :: answer

           answer => OBJ%NEXT
      END FUNCTION Dfoo
END MODULE Mod1
PROGRAM diagC448
      USE Mod1
      IMPLICIT NONE
END PROGRAM diagC448
