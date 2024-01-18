!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution based on rank using NOPASS
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!*  R448 type-bound-procedure-part is contains-stmt
!*                                     [ binding-private-stmt ]
!*                                     proc-binding-stmt
!*                                     [ proc-binding-stmt ] ...
!*
!*  R450 proc-binding-stmt is specific-binding
!*                         or generic-binding
!*                         or final-binding
!*
!*  R451 specific-binding is PROCEDURE [ (interface-name) ] &
!*                                    & [ [, binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(:), ALLOCATABLE :: tag

        CONTAINS
         PROCEDURE, NOPASS :: sub1
         PROCEDURE, NOPASS :: sub2
         GENERIC :: SUB =>  sub1 , sub2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN :: l2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3
      END TYPE NextGen

      CONTAINS
!*
      SUBROUTINE sub1(pntr,Obj)
      CLASS(Base(4,*)) :: Obj         ! Obj: rank is 0
      CLASS(Base(4,:)), POINTER, INTENT(OUT) :: pntr

      ALLOCATE (pntr,  source = Obj)
      IF (.NOT. ASSOCIATED(pntr)) ERROR STOP 1

      pntr%tag = 'sub1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(pntr,Obj)
      CLASS(Base(4,*)) :: Obj(:)      ! Obj: rank is 1
      CLASS(Base(4,:)), POINTER, INTENT(OUT) :: pntr

      ALLOCATE (pntr,  source = Obj(1))
      IF (.NOT. ASSOCIATED(pntr)) ERROR STOP 2

      pntr%tag = 'sub2'

      END SUBROUTINE sub2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound01e
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1

      TYPE(Base(4,5))  :: base1
      TYPE(Child(4,5,4,10)), TARGET :: tgt1
      TYPE(child(4,10,4,20)) :: child1
      TYPE(NextGen(4,10,4,20,4,40)) :: nxtg

      TYPE(Base(4,5))  :: arr_base(10)
      TYPE(child(4,10,4,15)) :: arr_child(10)
      TYPE(NextGen(4,10,4,15,4,15)) :: arr_nxtg(10)
      TYPE(NextGen(4,10,4,15,4,15)), TARGET :: tgt_nxtg(100)
!*
!  The following will call sub1
!*
      poly1 => tgt1
      call base1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 10
      IF (poly1%tag .NE. 'sub1') ERROR STOP 11

      deallocate (poly1)

      call base1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 12
      IF (poly1%tag .NE. 'sub1') ERROR STOP 13

      deallocate (poly1)

      call base1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 14
      IF (poly1%tag .NE. 'sub1') ERROR STOP 15

      deallocate (poly1)

      call base1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 16
      IF (poly1%tag .NE. 'sub1') ERROR STOP 17

      deallocate (poly1)

      call child1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 18
      IF (poly1%tag .NE. 'sub1') ERROR STOP 19

      deallocate (poly1)

      call child1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 20
      IF (poly1%tag .NE. 'sub1') ERROR STOP 21

      deallocate (poly1)

      call child1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 22
      IF (poly1%tag .NE. 'sub1') ERROR STOP 23

      deallocate (poly1)

      call child1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 24
      IF (poly1%tag .NE. 'sub1') ERROR STOP 25

      deallocate (poly1)

      call tgt1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 26
      IF (poly1%tag .NE. 'sub1') ERROR STOP 27

      deallocate (poly1)

      call tgt1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 28
      IF (poly1%tag .NE. 'sub1') ERROR STOP 29

      deallocate (poly1)

      call tgt1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 30
      IF (poly1%tag .NE. 'sub1') ERROR STOP 31

      deallocate (poly1)

      call tgt1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 32
      IF (poly1%tag .NE. 'sub1') ERROR STOP 33

      deallocate (poly1)

      call poly1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 34
      IF (poly1%tag .NE. 'sub1') ERROR STOP 35

      deallocate (poly1)

      call poly1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 36
      IF (poly1%tag .NE. 'sub1') ERROR STOP 37

      deallocate (poly1)

      call poly1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 38
      IF (poly1%tag .NE. 'sub1') ERROR STOP 39

      deallocate (poly1)

      call poly1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 40
      IF (poly1%tag .NE. 'sub1') ERROR STOP 41

      deallocate (poly1)

      call poly1%SUB(poly1,poly1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 42
      IF (poly1%tag .NE. 'sub1') ERROR STOP 43

      deallocate (poly1)

      ALLOCATE(NextGen(4,10,8,100,8,100):: poly1)
      call base1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 44
      IF (poly1%tag .NE. 'sub1') ERROR STOP 45

      deallocate (poly1)

      call base1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 46
      IF (poly1%tag .NE. 'sub1') ERROR STOP 47

      deallocate (poly1)

      call base1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 48
      IF (poly1%tag .NE. 'sub1') ERROR STOP 49

      deallocate (poly1)

      call base1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 50
      IF (poly1%tag .NE. 'sub1') ERROR STOP 51

      deallocate (poly1)

      call child1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 52
      IF (poly1%tag .NE. 'sub1') ERROR STOP 53

      deallocate (poly1)

      call child1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 54
      IF (poly1%tag .NE. 'sub1') ERROR STOP 55

      deallocate (poly1)

      call child1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 56
      IF (poly1%tag .NE. 'sub1') ERROR STOP 57

      deallocate (poly1)

      call child1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 58
      IF (poly1%tag .NE. 'sub1') ERROR STOP 59

      deallocate (poly1)

      call tgt1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 60
      IF (poly1%tag .NE. 'sub1') ERROR STOP 61

      deallocate (poly1)

      call tgt1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 62
      IF (poly1%tag .NE. 'sub1') ERROR STOP 63

      deallocate (poly1)

      call tgt1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 64
      IF (poly1%tag .NE. 'sub1') ERROR STOP 65

      deallocate (poly1)

      call tgt1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 66
      IF (poly1%tag .NE. 'sub1') ERROR STOP 67

      deallocate (poly1)

      call poly1%SUB(poly1,base1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 68
      IF (poly1%tag .NE. 'sub1') ERROR STOP 69

      deallocate (poly1)

      call poly1%SUB(poly1,child1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 70
      IF (poly1%tag .NE. 'sub1') ERROR STOP 71

      deallocate (poly1)

      call poly1%SUB(poly1,tgt1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 72
      IF (poly1%tag .NE. 'sub1') ERROR STOP 73

      deallocate (poly1)

      call poly1%SUB(poly1,nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 74
      IF (poly1%tag .NE. 'sub1') ERROR STOP 75

      deallocate (poly1)

      call poly1%SUB(poly1,poly1)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 76
      IF (poly1%tag .NE. 'sub1') ERROR STOP 77

      deallocate (poly1)
!*
!  The following will call sub2
!*
      call arr_base%SUB(poly1,arr_base)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 78
      IF (poly1%tag .NE. 'sub2') ERROR STOP 79

      deallocate (poly1)

      call base1%SUB(poly1,arr_child)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 80
      IF (poly1%tag .NE. 'sub2') ERROR STOP 81

      deallocate (poly1)

      call base1%SUB(poly1,tgt_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 82
      IF (poly1%tag .NE. 'sub2') ERROR STOP 83

      deallocate (poly1)

      call base1%SUB(poly1,arr_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 84
      IF (poly1%tag .NE. 'sub2') ERROR STOP 85

      deallocate (poly1)

      call child1%SUB(poly1,arr_base)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 86
      IF (poly1%tag .NE. 'sub2') ERROR STOP 87

      deallocate (poly1)

      call child1%SUB(poly1,arr_child)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 88
      IF (poly1%tag .NE. 'sub2') ERROR STOP 89

      deallocate (poly1)

      call child1%SUB(poly1,tgt_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 90
      IF (poly1%tag .NE. 'sub2') ERROR STOP 91

      deallocate (poly1)

      call arr_child%SUB(poly1,arr_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 92
      IF (poly1%tag .NE. 'sub2') ERROR STOP 93

      deallocate (poly1)

      call arr_child%SUB(poly1,arr_child)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 94
      IF (poly1%tag .NE. 'sub2') ERROR STOP 95

      deallocate (poly1)

      call tgt_nxtg%SUB(poly1,arr_base)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 96
      IF (poly1%tag .NE. 'sub2') ERROR STOP 97

      deallocate (poly1)

      call tgt_nxtg%SUB(poly1,arr_child)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 98
      IF (poly1%tag .NE. 'sub2') ERROR STOP 99

      deallocate (poly1)

      call tgt_nxtg%SUB(poly1,tgt_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 100
      IF (poly1%tag .NE. 'sub2') ERROR STOP 101

      deallocate (poly1)

      call tgt_nxtg%SUB(poly1,arr_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 102
      IF (poly1%tag .NE. 'sub2') ERROR STOP 103

      deallocate (poly1)

      call poly1%SUB(poly1,arr_base)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 104
      IF (poly1%tag .NE. 'sub2') ERROR STOP 105

      deallocate (poly1)

      call poly1%SUB(poly1,arr_child)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 106
      IF (poly1%tag .NE. 'sub2') ERROR STOP 107

      deallocate (poly1)

      call poly1%SUB(poly1,tgt_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 108
      IF (poly1%tag .NE. 'sub2') ERROR STOP 109

      deallocate (poly1)

      call poly1%SUB(poly1,arr_nxtg)
      IF ( .NOT. ASSOCIATED(poly1) ) ERROR STOP 110
      IF (poly1%tag .NE. 'sub2') ERROR STOP 111

      END PROGRAM Generic_TypeBound01e
