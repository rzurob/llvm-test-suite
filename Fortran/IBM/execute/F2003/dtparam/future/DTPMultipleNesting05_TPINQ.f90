!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTPMultipleNesting05
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : February 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Run Time Offset (RTO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* defect 361707
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Mod
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        CHARACTER(l1)      :: tag(l1)
        INTEGER(k1), ALLOCATABLE :: my_arr(:)  
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        CLASS(Base(k2,l2)), ALLOCATABLE :: b1   
      END TYPE Child

      TYPE, EXTENDS(Child) :: Branch  (k3,l3,l4,l5)
        INTEGER, KIND :: k3 
        INTEGER, LEN  :: l3, l4, l5

        TYPE(Base(k3,l4)) :: cmp1   
        TYPE(Base(k3,l5)) :: cmp2 
      END TYPE Branch

      CONTAINS

      SUBROUTINE verify_type_param(Arg)
         CLASS(Base(4,*)) :: Arg    

         SELECT TYPE ( Arg )  
              CLASS IS (Base(4,*))
                 Arg%tag = 'Base'
                 IF (Arg%l1 .NE. 8) STOP 20

              CLASS IS (Child(4,*,4,*))
                 Arg%tag = 'Child'
                 IF (Arg%l1 .NE. 10) STOP 21
                 IF (Arg%l2 .NE. 8) STOP 22

              CLASS IS (Branch(4,*,4,*,4,*,*,*))
                 Arg%tag = 'Branch'
                 IF (Arg%l1 .NE. 10) STOP 23
                 IF (Arg%l2 .NE. 8) STOP 24
                 IF (Arg%l3 .NE. 5) STOP 25
                 IF (Arg%l4 .NE. 8) STOP 26
                 IF (Arg%l5 .NE. 8) STOP 27

              CLASS DEFAULT
                 STOP 28
         END SELECT

      END SUBROUTINE verify_type_param

      SUBROUTINE verify_my_arr(Arr, k) 
         INTEGER :: k, Arr(:)  
 
         IF ( ANY(Arr .NE. k))  STOP 100 

      END SUBROUTINE verify_my_arr    
END MODULE Mod

PROGRAM DTPMultipleNesting05
      USE Mod
      IMPLICIT NONE

      INTEGER :: i 
      CLASS(Base(4,:)), POINTER :: upoly

      IF ( ASSOCIATED(upoly) ) STOP 10

!*   Base

      ALLOCATE(Base(4,8) :: upoly)
      CALL verify_type_param(upoly)
      IF (ANY(upoly%tag .NE. 'Base')) STOP 11

      ALLOCATE( upoly%my_arr(upoly%l1), SOURCE = (/(1, i = 1, upoly%l1)/) )
      IF ( SIZE(upoly%my_arr) .NE. 8 ) STOP 30 
      CALL verify_my_arr( upoly%my_arr, 1 ) 

!*   Child 

      ALLOCATE(Child(4,10,4,8) :: upoly)
      CALL verify_type_param(upoly)
      IF (ANY(upoly%tag .NE. 'Child')) STOP 13

      SELECT TYPE ( upoly )
         CLASS IS (Child(4,*,4,*))
             ALLOCATE( upoly%my_arr(upoly%l2), SOURCE = (/(2, i = 1, upoly%l2)/) )
             IF ( SIZE(upoly%my_arr) .NE. 8 ) STOP 31 

             IF ( ALLOCATED(upoly%b1) ) STOP 32
             ALLOCATE( Base(4,upoly%l2) :: upoly%b1 ) 
             CALL verify_type_param(upoly%b1)
             IF (ANY(upoly%b1%tag .NE. 'Base')) STOP 33

             ALLOCATE( upoly%b1%my_arr(upoly%b1%l1), SOURCE = (/(3, i = 1, upoly%b1%l1)/) )
             IF ( SIZE(upoly%b1%my_arr) .NE. 8 ) STOP 34 

             CALL verify_my_arr( upoly%b1%my_arr, 3 ) 
             CALL verify_my_arr( upoly%my_arr, 2 ) 

         CLASS DEFAULT
             STOP 35
      END SELECT

!*   Branch 

      ALLOCATE(Branch(4,10,4,8,4,5,8,8) :: upoly)
      CALL verify_type_param(upoly)
      IF (ANY(upoly%tag .NE. 'Branch')) STOP 15

      SELECT TYPE ( upoly )
         CLASS IS (Branch(4,*,4,*,4,*,*,*))
            ALLOCATE( upoly%my_arr(upoly%l3), SOURCE = (/(4, i = 1, upoly%l3)/) )
            IF ( SIZE(upoly%my_arr) .NE. 5 ) STOP 35

            IF ( ALLOCATED(upoly%b1) ) STOP 36

            ALLOCATE( Base(4,upoly%l2) :: upoly%b1) 
            ALLOCATE( upoly%b1%my_arr(upoly%b1%l1), SOURCE = (/(5, i = 1, upoly%b1%l1)/) )
            IF ( SIZE(upoly%b1%my_arr) .NE. 8) STOP 39 

            ALLOCATE( upoly%cmp1%my_arr(upoly%cmp1%l1), SOURCE = (/(6, i = 1, upoly%cmp1%l1)/) )
            IF ( SIZE(upoly%cmp1%my_arr) .NE. 8 ) STOP 40 

            ALLOCATE( upoly%cmp2%my_arr(upoly%cmp2%l1), SOURCE = (/(7, i = 1, upoly%cmp2%l1)/) )
            IF ( SIZE(upoly%cmp2%my_arr) .NE. 8 ) STOP 41 

            CALL verify_type_param(upoly%b1)
            IF ( ANY(upoly%b1%tag .NE. 'Base') ) STOP 42

            CALL verify_type_param(upoly%cmp1)
            IF ( ANY(upoly%cmp1%tag .NE. 'Base') ) STOP 43

            CALL verify_type_param(upoly%cmp2)
            IF ( ANY(upoly%cmp2%tag .NE. 'Base') ) STOP 44

            CALL verify_my_arr( upoly%cmp2%my_arr, 7 ) 
            CALL verify_my_arr( upoly%cmp1%my_arr, 6 ) 
            CALL verify_my_arr(upoly%b1%my_arr, 5) 
            CALL verify_my_arr( upoly%my_arr, 4 ) 
         CLASS DEFAULT
             STOP 45
      END SELECT

      DEALLOCATE(upoly)

END PROGRAM DTPMultipleNesting05
