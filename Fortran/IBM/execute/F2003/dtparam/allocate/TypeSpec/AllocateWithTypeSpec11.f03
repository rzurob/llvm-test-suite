!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!* C626 (R623) A type-param-value in a type-spec shall be an asterisk if and only if
!* each allocate-object is a dummy argument for which the corresponding type parameter is assumed.
!*
!* Defect 361720
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithTypeSpec11
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 1

        INTEGER(KIND=k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 2

        CLASS(Base(k2,l2)), POINTER :: b_cmp
        CLASS(Base(k2,l2)), POINTER :: c_cmp
      END TYPE Child

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10, len2 = 5
      INTEGER :: I, J

      CLASS(Base(knd1,len1)), POINTER :: b1
      CLASS(Base(knd1,len2)), POINTER :: b2

      CALL allocate_base(b1, len1)

      CALL allocate_child(b2, 100)

      call verifyB1

      call verifyB2 (100)

      CONTAINS

      subroutine verifyB1
        integer i
        if (.not. associated(b1)) error stop 30
        do i = 1, len1
            if (b1%my_arr(i) /= i) error stop 31
        end do
      end subroutine

      subroutine verifyB2(depth)
        integer, intent(in) :: depth
        integer i,j
        class(base(knd1,:)), pointer :: iterator1, iterator2, tmp

        if (.not. associated(b2)) error stop 33

        do j = 1, len2
            if (b2%my_arr(j) /= j) error stop 50
        end do

        select type (b2)
            type is (child(knd1, *, knd1,*))
                iterator1 => b2%b_cmp
                iterator2 => b2%c_cmp
            class default
                stop 34
        end select

        do i = depth, 2, -1
            if (.not. associated(iterator1)) error stop 35
            if (.not. associated(iterator2)) error stop 36

            do j = 1, len2
                if (iterator1%my_arr(j) /= j) error stop 37
                if (iterator2%my_arr(j) /= j) error stop 38
            end do

            tmp => iterator2

            select type(tmp)
                type is (child(knd1,*,knd1,*))
                    iterator1 => tmp%b_cmp
                    iterator2 => tmp%c_cmp
                class default
                    stop 40
            end select
        end do
      end subroutine

      SUBROUTINE allocate_base(Arg, l)
      CLASS(Base(knd1,*)), POINTER  :: Arg
      integer, intent(in) :: l

      ALLOCATE(Base(knd1,*) :: Arg)
      IF ( Arg%l1 .NE. l) ERROR STOP 20

      Arg%my_arr = (/(i, i = 1, Arg%l1)/)

      END SUBROUTINE allocate_base

      RECURSIVE SUBROUTINE allocate_child(Arg, depth)
      CLASS(Base(knd1,*)), POINTER :: Arg

      integer, intent(in) :: depth

      if (depth <= 0) error stop 25

      ALLOCATE(Child(knd1,len2,knd1,len2) :: Arg)

      SELECT TYPE ( Arg )
        CLASS IS (Child(knd1,*,knd1,*))
            IF ( Arg%l1 .NE. len2) ERROR STOP 21
            IF ( Arg%l2 .NE. len2) ERROR STOP 22
            Arg%my_arr = (/(i, i = 1, Arg%l1)/)

            if (depth > 1) then
                call allocate_base(Arg%b_cmp,len2)
                call allocate_child(Arg%c_cmp, depth - 1)
            end if

        CLASS DEFAULT
           STOP 23
      END SELECT

      END SUBROUTINE allocate_child
END PROGRAM AllocateWithTypeSpec11
