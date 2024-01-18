!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_Interface05
!*                               DTP - Generic Interface
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : apparent ambiguity between elemental and nonelemental procedure
!*                               The reference is to the nonelemental procedure
!*                               
!*                     
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2 
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3 
      END TYPE NextGen

      INTERFACE FUNC
         module procedure foo1
         module procedure foo2
      END INTERFACE

      CONTAINS 
!*
      ELEMENTAL FUNCTION foo1(Obj,Arg)
      CLASS(Base(4,*)), INTENT(IN) :: Obj, Arg         ! Arg: rank is 0 
      CHARACTER(20)  :: foo1
      
      foo1 = "ele"       

      END FUNCTION foo1

      FUNCTION foo2(Obj,Arg)
      CLASS(Base(4,*)), INTENT(IN) :: Obj, Arg(*)      ! Arg: rank is 1 
      !TYPE(Base(4,20)) :: foo2
      CHARACTER(20)  :: foo2

      foo2 = "non-ele"       

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_Interface05
      USE MOD1
      IMPLICIT NONE 

      CLASS(Base(4,:)), POINTER :: poly1 
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(NextGen(4,10,4,4)) :: nxtg

      TYPE(child(4,10,4)) :: arr_child(10)
      TYPE(NextGen(4,10,4,4)) :: arr_nxtg(10)

      TYPE(Base(4,5)), TARGET :: tgt_base(1)
      TYPE(Child(4,5,4)), TARGET :: tgt_chd(2,2)
      TYPE(NextGen(4,10,4,4)), TARGET :: tgt_nxtg(100)

      CLASS(Base(4,:)), POINTER :: ptr_base(:)
      CLASS(Child(4,:,4)), POINTER :: ptr_chd(:,:)
      CLASS(NextGen(4,:,4,4)), POINTER :: ptr_nxtg(:)

!*
!  The following will call foo1 
!*
      IF( FUNC(base1,base1) .NE. "ele" ) STOP 10
      IF( FUNC(base1,tgt1) .NE. "ele" ) STOP 11
      IF( FUNC(base1,nxtg) .NE. "ele" ) STOP 12

      ALLOCATE(Base(4,10):: poly1)      
      IF( FUNC(poly1,base1) .NE. "ele" ) STOP 13
      IF( FUNC(poly1,tgt1) .NE. "ele" ) STOP 14
      IF( FUNC(poly1,nxtg) .NE. "ele" ) STOP 15
      IF( FUNC(poly1,poly1) .NE. "ele" ) STOP 16

      poly1 => tgt1                       
      IF( FUNC(poly1,base1) .NE. "ele" ) STOP 17
      IF( FUNC(poly1,tgt1) .NE. "ele" ) STOP 18
      IF( FUNC(poly1,nxtg) .NE. "ele" ) STOP 19
      IF( FUNC(poly1,poly1) .NE. "ele" ) STOP 20

!*
!  The following will call foo2 
!*
      IF( FUNC(nxtg,arr_child) .NE. "non-ele" ) STOP 21
      IF( FUNC(poly1,arr_child) .NE. "non-ele" ) STOP 22
      IF( FUNC(poly1,arr_nxtg) .NE. "non-ele" ) STOP 23

      ALLOCATE(child(4,10,4) :: ptr_base(10), ptr_chd(5,5))
      IF( FUNC(base1,ptr_base) .NE. "non-ele" ) STOP 24
      IF( ANY(FUNC(poly1,ptr_chd) .NE. "ele" )) STOP 25
      IF( ANY(FUNC(nxtg,ptr_chd) .NE. "ele" )) STOP 26

      ptr_base => tgt_base; ptr_chd => tgt_chd; ptr_nxtg => tgt_nxtg
      IF( FUNC(base1,ptr_base) .NE. "non-ele" ) STOP 24
      IF( FUNC(nxtg,ptr_base) .NE. "non-ele" ) STOP 24
      IF( FUNC(poly1,ptr_base) .NE. "non-ele" ) STOP 24
      IF( ANY(FUNC(base1,ptr_chd) .NE. "ele" )) STOP 25
      IF( ANY(FUNC(nxtg,ptr_chd) .NE. "ele" )) STOP 26
      IF( ANY(FUNC(poly1,ptr_chd) .NE. "ele" )) STOP 25

     END PROGRAM  Generic_Interface05
