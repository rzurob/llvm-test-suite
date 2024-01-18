!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type04e - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : September 04, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association - Select case within select type
!*                               Selector being a function call
!*                               
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION                :
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!*  R823 type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS DEFAULT [ select-construct-name ]
!*
!* see defect 355925
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Node  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 

        PRIVATE
        CHARACTER(l1), public :: Grad
        CHARACTER(l1) :: Notes 
      END TYPE Node 

      TYPE, EXTENDS(Node) :: ExtNode
        PRIVATE
        INTEGER(k1), ALLOCATABLE :: Ext_array(:)
      END TYPE ExtNode 

      TYPE, EXTENDS(Node) :: BrcNode
        PRIVATE
        INTEGER(k1), ALLOCATABLE :: Brc_array(:)
      END TYPE BrcNode 

      INTEGER, PARAMETER :: N = 3
      INTEGER, PARAMETER :: knd1 = 4, len1 = 10 , knd2 = 8, len2 = 20

      CONTAINS

      SUBROUTINE fill_arr(T)
         CLASS(*) ::  T

         SELECT TYPE ( T )
           CLASS IS (ExtNode(knd1,*))
            IF ( .NOT. ALLOCATED(T%Ext_array) ) ALLOCATE( T%Ext_array(N) )
            T%Ext_array = (/ 76, 65, 81 /)

           CLASS IS (BrcNode(knd2,*))
            IF ( .NOT. ALLOCATED(T%Brc_array) ) ALLOCATE( T%Brc_array(N) )
            T%Brc_array = (/ 88, 91, 95 /)

           CLASS DEFAULT
              STOP 20
         END SELECT
      END SUBROUTINE fill_arr

      SUBROUTINE clean_arr(T)
         CLASS(*) ::  T

         SELECT TYPE ( T )
           CLASS IS (ExtNode(knd1,*))
            IF ( ALLOCATED(T%Ext_array) ) DEALLOCATE (T%Ext_array)

           CLASS IS (BrcNode(knd2,*))
            IF ( ALLOCATED(T%Brc_array) ) DEALLOCATE (T%Brc_array)

           CLASS DEFAULT
              STOP 21
         END SELECT
      END SUBROUTINE clean_arr

      FUNCTION make_decision(U,V) result(decision)
         CLASS(*), POINTER, OPTIONAL ::  U, V
         REAL    :: tmp1
         LOGICAL :: decision  

         IF ( .NOT. PRESENT(U) .OR. .NOT. PRESENT(V) ) THEN
           decision = .False.
           return
         ENDIF

         SELECT TYPE ( U )
           CLASS IS (ExtNode(knd1,*))
              tmp1 = ( SUM(U%Ext_array) ) /3
              IF ( FLOOR(tmp1) .GE. 70 .AND. FLOOR(tmp1) .LE. 76 ) THEN
                 U%Grad = 'C'
                 U%Notes = 'Average'
              END IF

           CLASS IS (BrcNode(knd2,*))
              STOP 22

           CLASS DEFAULT
              STOP 23
         END SELECT

         SELECT TYPE ( V )
           CLASS IS (ExtNode(knd1,*))
              STOP 24

           CLASS IS (BrcNode(knd2,*))
              tmp1 = ( SUM(V%Brc_array) ) /3
              IF ( FLOOR(tmp1) .GE. 90 .AND. FLOOR(tmp1) .LE. 94 ) THEN
                 V%Grad = 'A-'
                 V%Notes = 'Excellent!'
              END IF

           CLASS DEFAULT
              STOP 25
         END SELECT

         decision = .True.
      END Function
END MODULE Mod1
!*
MODULE Mod2
      USE Mod1
      IMPLICIT NONE

      CONTAINS 

      SUBROUTINE Sub1(T)
         CLASS(*), POINTER ::  T

         SELECT TYPE ( A => T )
           CLASS IS (ExtNode(knd1,*))
             IF (A%k1 .NE. knd1) STOP 112
             IF (A%l1 .NE. len1) STOP 113

             CALL fill_arr(A)

           CLASS IS (BrcNode(knd2,*))
             IF (A%k1 .NE. knd2) STOP 114
             IF (A%l1 .NE. len2) STOP 115

             CALL fill_arr(A)

           CLASS DEFAULT
              STOP 30

         END SELECT 
      END SUBROUTINE

      SUBROUTINE give_me_grad(U,V)
         CLASS(*), POINTER ::  U, V

         CALL Sub1(U)
         CALL Sub1(V)

         IF ( .NOT.  make_decision(U,V) ) STOP 116

         CALL clean_arr(U)
         CALL clean_arr(V)

      END SUBROUTINE
END MODULE Mod2
!*
PROGRAM Select_Type04e
      USE Mod1
      USE Mod2, ONLY: give_me_grad
      IMPLICIT NONE 

      TYPE(ExtNode(knd1,len1)), TARGET  :: FirstSubj 
      TYPE(BrcNode(knd2,len2)), TARGET  :: SecondSubj
      CLASS(*), POINTER ::  SubjA, SubjB 

      SubjA  => FirstSubj
      SubjB  => SecondSubj
      IF ( .NOT. ASSOCIATED(SubjA)) STOP 10
      IF ( .NOT. ASSOCIATED(SubjB)) STOP 11

      CALL give_me_grad(SubjA,SubjB)
END PROGRAM Select_Type04e
