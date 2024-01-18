!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to external subroutine with passed-object dummy argument, DTP arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassDTPInterfaceKLSub (<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to external subroutines
!*  which do expect a passed-object dummy argument.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.  The arg which is passed in has
!*  kind and len parameters.
!*
!*  Here, the type is a sequence type and the subroutines are external (the
!*  name dtpPPCLocalPassDTPInterfaceKLSubExt is incorrect, and should be
!*  changed to dtpPPCLocalNPassDTPInterfaceKLSubExtSequence).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalPassDTPInterfaceKLSubExtmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     sequence
     character(l)  :: chval
     integer(k)    :: ival
     procedure (s1), pointer, nopass :: p1 => null()
     procedure (s2), pointer, nopass :: p2 => null()
     procedure (s4), pointer, nopass :: p4 => null()
  end type dt

  abstract interface

     subroutine s1(a1)
       import :: dt
       type(dt(1,*)), intent(in) :: a1
     end subroutine s1

     subroutine s2(a1)
       import :: dt
       type(dt(2,*)), intent(in) :: a1
     end subroutine s2

     subroutine s4(a1)
       import :: dt
       type(dt(4,*)), intent(in) :: a1
     end subroutine s4

  end interface

end module dtpPPCLocalPassDTPInterfaceKLSubExtmod


program dtpPPCLocalPassDTPInterfaceKLSubExt

  use dtpPPCLocalPassDTPInterfaceKLSubExtmod
  implicit none
  type(dt(1,0)) :: t1a, t1b
  type(dt(2,4)) :: t2a, t2b
  type(dt(1,3)) :: t1c, t1d
  type(dt(1,:)) :: t1p
  type(dt(4,:)) :: t4p
  target  :: t1c
  pointer :: t1p, t4p

 interface
    subroutine disp1(this)
      import :: dt
      type(dt(1,*)), intent(in) :: this
    end subroutine disp1
    subroutine disp2(this)
      import :: dt
      type(dt(2,*)), intent(in) :: this
    end subroutine disp2
    subroutine disp4(this)
      import :: dt
      type(dt(4,*)), intent(in) :: this
    end subroutine disp4
    subroutine disp1a(this)
      import :: dt
      type(dt(1,*)), intent(in) :: this
    end subroutine disp1a
    subroutine disp2a(this)
      import :: dt
      type(dt(2,*)), intent(in) :: this
    end subroutine disp2a
  end interface

  t1a = dt(1,0)("",127,disp1,disp2,disp4)
  t1b = dt(1,0)("",-127,disp1a,disp2a,disp4)
  t2a = dt(2,4)("abcd",32000,disp1,disp2,disp4)
  t2b = dt(2,4)("efgh",-12345,disp1a,disp2a,disp4)
  t1c = dt(1,3)("ijk",1,disp1,disp2,disp4)
  t1d = dt(1,3)("lmn",-1,disp1a,disp2a,disp4)
  t1p => t1c

  call t1a % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t1a % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t1a % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t1a % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t1a % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1
  call t1a % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t1a % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  call t1b % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t1b % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t1b % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t1b % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t1b % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1a
  call t1b % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t1b % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  call t2a % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t2a % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t2a % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t2a % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t2a % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1
  call t2a % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t2a % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  call t2b % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t2b % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t2b % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t2b % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t2b % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1a
  call t2b % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t2b % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  call t1c % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t1c % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t1c % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t1c % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t1c % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1
  call t1c % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t1c % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  call t1d % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t1d % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t1d % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t1d % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t1d % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1a
  call t1d % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t1d % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  call t1p % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t1p % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t1p % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t1p % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t1p % p1(t1p) ! 1 1 1 3 3 >ijk< in disp1
  call t1p % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t1p % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  t1a % p1 => disp1a
  t1a % p2 => disp2a

  t1b % p1 => disp1
  t1b % p2 => disp2

  t1c % p1 => disp1a
  t1c % p2 => disp2a

  t1d % p1 => disp1
  t1d % p2 => disp2

  t2a % p1 => disp1a
  t2a % p2 => disp2a

  t2b % p1 => disp1
  t2b % p2 => disp2

  allocate(t1p, source=dt(1,5)("opqrs",0,disp1a,disp2a,disp4))

  call t1a % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t1a % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t1a % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t1a % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t1a % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1a
  call t1a % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t1a % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  call t1b % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t1b % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t1b % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t1b % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t1b % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1
  call t1b % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t1b % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  call t2a % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t2a % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t2a % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t2a % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t2a % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1a
  call t2a % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t2a % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  call t2b % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t2b % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t2b % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t2b % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t2b % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1
  call t2b % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t2b % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  call t1c % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t1c % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t1c % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t1c % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t1c % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1a
  call t1c % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t1c % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  call t1d % p1(t1a) ! 127 1 1 0 0 >< in disp1
  call t1d % p1(t1b) ! -127 1 1 0 0 >< in disp1
  call t1d % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1
  call t1d % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1
  call t1d % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1
  call t1d % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2
  call t1d % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2
  print *

  call t1p % p1(t1a) ! 127 1 1 0 0 >< in disp1a
  call t1p % p1(t1b) ! -127 1 1 0 0 >< in disp1a
  call t1p % p1(t1c) ! 1 1 1 3 3 >ijk< in disp1a
  call t1p % p1(t1d) ! -1 1 1 3 3 >lmn< in disp1a
  call t1p % p1(t1p) ! 0 1 1 5 5 >opqrs< in disp1a
  call t1p % p2(t2a) ! 32000 2 2 4 4 >abcd< in disp2a
  call t1p % p2(t2b) ! -12345 2 2 4 4 >efgh< in disp2a
  print *

  allocate(t4p, source=dt(4,5)("opqrs",123454321,disp1,disp2,disp4))
  call t4p % p4(t4p) ! 123454321 4 4 5 5 >pqr< in disp4
  call t4p % p4(dt(4,3)("tuv", 543212345, disp1a, disp2a, disp4)) ! 543212345 4 4 3 3 >u< in disp4
  print *

  print *, "done"

end program dtpPPCLocalPassDTPInterfaceKLSubExt


subroutine disp1(this)
  use :: dtpPPCLocalPassDTPInterfaceKLSubExtmod
  type(dt(1,*)), intent(in) :: this
  print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp1"
end subroutine disp1


subroutine disp2(this)
  use :: dtpPPCLocalPassDTPInterfaceKLSubExtmod
  type(dt(2,*)), intent(in) :: this
  print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp2"
end subroutine disp2


subroutine disp4(this)
  use :: dtpPPCLocalPassDTPInterfaceKLSubExtmod
  type(dt(4,*)), intent(in) :: this
  print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval(2:this%l-1), "< in disp4"
end subroutine disp4


subroutine disp1a(this)
  use :: dtpPPCLocalPassDTPInterfaceKLSubExtmod
  type(dt(1,*)), intent(in) :: this
  print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp1a"
end subroutine disp1a


subroutine disp2a(this)
  use :: dtpPPCLocalPassDTPInterfaceKLSubExtmod
  type(dt(2,*)), intent(in) :: this
  print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp2a"
end subroutine disp2a
