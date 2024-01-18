!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-29 (original: 2006-08-09)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement no TS: as
!*                               selector in SELECT TYPE (all items are
!*                               polymorphic)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : polymorphic, select type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  AC's are can only be polymorphic if all ac_values are polymorphic and there
!*  is no type specifier.  Verify that a polymorphic AC can be used this way,
!*  and that the type is correct.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type :: derived (lderived_1,lderived_2,lderived_3) ! lderived_1,lderived_2,lderived_3=1,3,5
     integer, len :: lderived_1,lderived_2,lderived_3
  end type derived
end module mod

program acetnone09lll

  use mod
  implicit none
  integer :: j, expType, actualType
  logical :: error

  integer, target          :: i
  integer(1), target       :: i1
  integer(2), target       :: i2
  integer(4), target       :: i4
  integer(8), target       :: i8
  real, target             :: r
  real(4), target          :: r4
  real(8), target          :: r8
  double precision, target :: d
  complex, target          :: z
  complex(4), target       :: z4
  complex(8), target       :: z8
  logical, target          :: l
  logical(1), target       :: l1
  logical(2), target       :: l2
  logical(4), target       :: l4
  logical(8), target       :: l8
  character, target        :: ch
  type (derived(1,3,5)), target   :: t ! tcx: (1,3,5)

  class (*), pointer       :: p, p2

  enum, bind(c)
  enumerator :: ITYPE=0, ITYPE1=ITYPE+1, ITYPE2=ITYPE+2, ITYPE4=ITYPE+4, ITYPE8=ITYPE+8
  enumerator :: RTYPE, RTYPE4=RTYPE+4, RTYPE8=RTYPE+8 ! DTYPE would be same as RTYPE8
  enumerator :: ZTYPE, ZTYPE4=ZTYPE+4, ZTYPE8=ZTYPE+4
  enumerator :: LTYPE, LTYPE1=LTYPE+1, LTYPE2=LTYPE+2, LTYPE4=LTYPE+4, LTYPE8=LTYPE+8
  enumerator :: CHTYPE
  enumerator :: DTYPE
  enumerator :: UTYPE
  end enum

  error = .false.

  do j=1,19
     p  => rainbow(j,expType)

     select type ( o => (/ p /) )
!    ! Commented cases are duplicates of some specific kind of intrinsic:
!    type is (integer);      actualType = ITYPE
     type is (integer(1));   actualType = ITYPE1
     type is (integer(2));   actualType = ITYPE2
     type is (integer(4));   actualType = ITYPE4
     type is (integer(8));   actualType = ITYPE8
!    type is (real);         actualType = RTYPE
     type is (real(4));      actualType = RTYPE4
     type is (real(8));      actualType = RTYPE8
!    type is (double precision); actualType = RTYPE
!    type is (complex);      actualType = ZTYPE
     type is (complex(4));   actualType = ZTYPE4
     type is (complex(8));   actualType = ZTYPE8
!    type is (logical);      actualType = LTYPE
     type is (logical(1));   actualType = LTYPE1
     type is (logical(2));   actualType = LTYPE2
     type is (logical(4));   actualType = LTYPE4
     type is (logical(8));   actualType = LTYPE8
     type is (character(*)); actualType = CHTYPE
     type is (derived(*,*,*));      actualType = DTYPE ! tcx: (*,*,*)
     class default;          actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type: ", actualType, "should be", expType
        error = .true.
     end if

     ! Repeat for two-element AC

     p2 => rainbow(j,expType)
     select type ( o => (/ p, p2 /) )
!    ! Commented cases are duplicates of some specific kind of intrinsic:
!    type is (integer);      actualType = ITYPE
     type is (integer(1));   actualType = ITYPE1
     type is (integer(2));   actualType = ITYPE2
     type is (integer(4));   actualType = ITYPE4
     type is (integer(8));   actualType = ITYPE8
!    type is (real);         actualType = RTYPE
     type is (real(4));      actualType = RTYPE4
     type is (real(8));      actualType = RTYPE8
!    type is (double precision); actualType = RTYPE
!    type is (complex);      actualType = ZTYPE
     type is (complex(4));   actualType = ZTYPE4
     type is (complex(8));   actualType = ZTYPE8
!    type is (logical);      actualType = LTYPE
     type is (logical(1));   actualType = LTYPE1
     type is (logical(2));   actualType = LTYPE2
     type is (logical(4));   actualType = LTYPE4
     type is (logical(8));   actualType = LTYPE8
     type is (character(*)); actualType = CHTYPE
     type is (derived(*,*,*));      actualType = DTYPE ! tcx: (*,*,*)
     class default;          actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type (2): ", actualType, "should be", expType
        error = .true.
     end if

     ! Repeat with implied-do

     select type ( o => (/ (p, p2, j=1,2) /) )
!    ! Commented cases are duplicates of some specific kind of intrinsic:
!    type is (integer);      actualType = ITYPE
     type is (integer(1));   actualType = ITYPE1
     type is (integer(2));   actualType = ITYPE2
     type is (integer(4));   actualType = ITYPE4
     type is (integer(8));   actualType = ITYPE8
!    type is (real);         actualType = RTYPE
     type is (real(4));      actualType = RTYPE4
     type is (real(8));      actualType = RTYPE8
!    type is (double precision); actualType = RTYPE
!    type is (complex);      actualType = ZTYPE
     type is (complex(4));   actualType = ZTYPE4
     type is (complex(8));   actualType = ZTYPE8
!    type is (logical);      actualType = LTYPE
     type is (logical(1));   actualType = LTYPE1
     type is (logical(2));   actualType = LTYPE2
     type is (logical(4));   actualType = LTYPE4
     type is (logical(8));   actualType = LTYPE8
     type is (character(*)); actualType = CHTYPE
     type is (derived(*,*,*));      actualType = DTYPE ! tcx: (*,*,*)
     class default;          actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type (3): ", actualType, "should be", expType
        error = .true.
     end if

     ! Repeat with implied-do (size zero)

     select type ( o => (/ (p, p2, j=1,0) /) )
!    ! Commented cases are duplicates of some specific kind of intrinsic:
!    type is (integer);      actualType = ITYPE
     type is (integer(1));   actualType = ITYPE1
     type is (integer(2));   actualType = ITYPE2
     type is (integer(4));   actualType = ITYPE4
     type is (integer(8));   actualType = ITYPE8
!    type is (real);         actualType = RTYPE
     type is (real(4));      actualType = RTYPE4
     type is (real(8));      actualType = RTYPE8
!    type is (double precision); actualType = RTYPE
!    type is (complex);      actualType = ZTYPE
     type is (complex(4));   actualType = ZTYPE4
     type is (complex(8));   actualType = ZTYPE8
!    type is (logical);      actualType = LTYPE
     type is (logical(1));   actualType = LTYPE1
     type is (logical(2));   actualType = LTYPE2
     type is (logical(4));   actualType = LTYPE4
     type is (logical(8));   actualType = LTYPE8
     type is (character(*)); actualType = CHTYPE
     type is (derived(*,*,*));      actualType = DTYPE ! tcx: (*,*,*)
     class default;          actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type (4): ", actualType, "should be", expType
        error = .true.
     end if

  end do

  if (error) error stop 2

contains
  function rainbow(sel,expType)
    integer, intent(in)  :: sel
    integer, intent(out) :: expType
    class(*), pointer :: rainbow
    select case(sel)
    case (1); rainbow => i;  expType = ITYPE + kind(i)
    case (2); rainbow => i1; expType = ITYPE1
    case (3); rainbow => i2; expType = ITYPE2
    case (4); rainbow => i4; expType = ITYPE4
    case (5); rainbow => i8; expType = ITYPE8
    case (6); rainbow => r;  expType = RTYPE + kind(r)
    case (7); rainbow => r4; expType = RTYPE4
    case (8); rainbow => r8; expType = RTYPE8
    case (9); rainbow => d;  expType = RTYPE8
    case(10); rainbow => z;  expType = ZTYPE + kind(z)
    case(11); rainbow => z4; expType = ZTYPE4
    case(12); rainbow => z8; expType = ZTYPE8
    case(13); rainbow => l;  expType = LTYPE + kind(l)
    case(14); rainbow => l1; expType = LTYPE1
    case(15); rainbow => l2; expType = LTYPE2
    case(16); rainbow => l4; expType = LTYPE4
    case(17); rainbow => l8; expType = LTYPE8
    case(18); rainbow => ch; expType = CHTYPE
    case(19); rainbow => t;  expType = DTYPE
    case default; rainbow => null(); print *, "Wrong selector value: ", sel
    end select
  end function rainbow

end program acetnone09lll


! Extensions to introduce derived type parameters:
! type: derived - added parameters (lderived_1,lderived_2,lderived_3) to invoke with (1,3,5)/declare with (*,*,*) - 5 changes
