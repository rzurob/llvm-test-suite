!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_Interface02e
!*                               DTP - Generic Interface
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution based on rank
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
!* See defect 355371 for the ICE 
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        CHARACTER(l1) :: base_name
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2 

        CHARACTER(l1) :: child_name  
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3 

        CHARACTER(l1) :: nextg_name
      END TYPE NextGen

      INTERFACE SUB  
        SUBROUTINE sub0(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,*)) :: Obj        ! rank 0
         END SUBROUTINE sub0

        SUBROUTINE sub1(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,*)) :: Obj(:)     ! rank 1
         END SUBROUTINE sub1

         SUBROUTINE sub2(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,*)) :: Obj(:,:)   ! rank 2
         END SUBROUTINE sub2
      END INTERFACE

      END MODULE Mod1
!*
      SUBROUTINE sub0(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,*)) :: Obj

        SELECT TYPE ( Obj )
          TYPE IS (NextGen(4,*,4,4))
             Obj%base_name  = 'sub0-NextGen'
             Obj%child_name = 'sub0-NextGen'
             Obj%nextg_name = 'sub0-NextGen'

          TYPE IS (Child(4,*,4))
             Obj%base_name  = 'sub0-Child'
             Obj%child_name = 'sub0-Child'

          TYPE IS (Base(4,*))
             Obj%base_name  = 'sub0-Base'

          CLASS DEFAULT
           STOP 110
      END SELECT

      END SUBROUTINE sub0

      SUBROUTINE sub1(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,*)) :: Obj(:)

        SELECT TYPE ( Obj )
          TYPE IS (NextGen(4,*,4,4))
             Obj%base_name  = 'sub1-NextGen'
             Obj%child_name = 'sub1-NextGen'
             Obj%nextg_name = 'sub1-NextGen'

          TYPE IS (Child(4,*,4))
             Obj%base_name  = 'sub1-Child'
             Obj%child_name = 'sub1-Child'

          TYPE IS (Base(4,*))
             Obj%base_name  = 'sub1-Base'

          CLASS DEFAULT
           STOP 110
      END SELECT

      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,*)) :: Obj(:,:)

        SELECT TYPE ( Obj )
          TYPE IS (NextGen(4,*,4,4))
             Obj%base_name  = 'sub2-NextGen'
             Obj%child_name = 'sub2-NextGen'
             Obj%nextg_name = 'sub2-NextGen'

          TYPE IS (Child(4,*,4))
             Obj%base_name  = 'sub2-Child'
             Obj%child_name = 'sub2-Child'

          TYPE IS (Base(4,*))
             Obj%base_name  = 'sub2-Base'

          CLASS DEFAULT
           STOP 111
      END SELECT

      END SUBROUTINE sub2
!*
      PROGRAM Generic_Interface02e
      USE MOD1
      IMPLICIT NONE 

      INTEGER :: I

      TYPE(Child(4,5,4)) :: tgt0
      CLASS(Base(4,:)), ALLOCATABLE  :: base0
      
      TYPE(Child(4,5,4)), TARGET :: tgt1(10)
      CLASS(Base(4,:)), POINTER  :: base1(:)
      
      TYPE(Child(4,5,4)), TARGET :: tgt2(2,2)
      CLASS(Base(4,:)), ALLOCATABLE :: base2(:,:)
      
      call sub(tgt0)
      IF ( tgt0%base_name(1:4) .NE. 'sub0' ) STOP 10

      call sub(tgt1)
      IF ( tgt1(1)%base_name(1:4) .NE. 'sub1' ) STOP 11

      call sub(tgt2)
      IF ( tgt2(1,1)%base_name(1:4) .NE. 'sub2' ) STOP 12

      ALLOCATE(base0, source=tgt0)
      ALLOCATE(base1(10), source=tgt0)
      ALLOCATE(base2(3,3), source=tgt0)

      call sub(base0)
      IF ( base0%base_name(1:4) .NE. 'sub0' ) STOP 10

      call sub(base1)
      IF ( base1(1)%base_name(1:4) .NE. 'sub1' ) STOP 11

      call sub(base2)
      IF ( base2(1,1)%base_name(1:4) .NE. 'sub2' ) STOP 12

      DEALLOCATE(base0,base1,base2)

      ALLOCATE(base0, source=tgt1(1))
      base1 => tgt1 
      ALLOCATE(Base(4,10) :: base2(100,100))

      call sub(base0)
      IF ( base0%base_name(1:4) .NE. 'sub0' ) STOP 10

      call sub(base1)
      IF ( base1(1)%base_name(1:4) .NE. 'sub1' ) STOP 11

      call sub(base2)
      IF ( base2(100,100)%base_name(1:4) .NE. 'sub2' ) STOP 12

      END PROGRAM Generic_Interface02e
