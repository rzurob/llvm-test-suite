!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to subroutine, passed-object dummy argument + other DTP args
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassDTPInterfaceLSub (<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to subroutines which
!*  expect a passed-object dummy argument.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalPassDTPInterfaceLSubmod

  implicit none

  type dt (l)
     integer, len :: l
     character(l) :: chval
     integer      :: ival
     procedure (s1), pointer, pass :: p1 => null()
     procedure (s2), pointer, pass :: p2 => null()
     procedure (s3), pointer, pass :: p3 => null()
  end type dt

  type dt2 (k,l1,l2)
     integer, kind :: k
     integer, len  :: l1, l2
     character(kind=k,len=l1) :: charr(l2)
  end type dt2

  abstract interface

     subroutine s1(a1)
       import :: dt
       class(dt(*)), intent(in) :: a1
     end subroutine s1

     subroutine s2(a1, a2)
       import :: dt
       class(dt(*)), intent(in) :: a1, a2
     end subroutine s2

     subroutine s3(a1, a2, a3)
       import :: dt, dt2
       class(dt(*)), intent(in) :: a1, a2
       class(dt2(1,*,*)), intent(in) :: a3
     end subroutine s3

  end interface


contains


  subroutine disp1(this)
    class(dt(*)), intent(in) :: this
    print *, "in disp1:", this%ival, kind(this%ival), len(this%chval), this%l, ">", this%chval, "<"
  end subroutine disp1

  subroutine disp1a(this)
    class(dt(*)), intent(in) :: this
    print *, "in disp1a:", this%ival, kind(this%ival), len(this%chval), this%l, ">", this%chval, "<"
  end subroutine disp1a

  subroutine disp2(this,that)
    class(dt(*)), intent(in) :: this, that
    print *, "in disp2:", this%ival, kind(this%ival), len(this%chval), this%l, &
             that%ival, kind(that%ival), len(that%chval), that%l, ">", this%chval, "/", that%chval, "<"
  end subroutine disp2

  subroutine disp2a(this,that)
    class(dt(*)), intent(in)   :: this, that
    type (dt(this%l+that%l+3)) :: tmp
    character (this%l+that%l+3) :: tmpstring
    write(tmpstring,"(3a)") "2a:", this%chval, that%chval
    tmp = dt(this%l+that%l+3)(tmpstring,this%ival+that%ival,this%p1,this%p2,this%p3)
    call tmp % p1
  end subroutine disp2a

  subroutine disp3(this,that,that2)
    class(dt(*)), intent(in) :: this, that
    class(dt2(1,*,*)), intent(in) :: that2
    integer :: i
    print *, "in disp3:", that2%k, kind(that2%charr), that2%l1, len(that2%charr), that2%l2, size(that2%charr), &
             "(/", (that2%charr(i), "/", i=1, that2%l2), ")", &
             this%ival, kind(this%ival), len(this%chval), this%l, &
             that%ival, kind(that%ival), len(that%chval), that%l, ">", this%chval, "/", that%chval, "<"
  end subroutine disp3

  subroutine disp3a(this,that,that2)
    class(dt(*)), intent(in) :: this, that
    class(dt2(1,*,*)), intent(in) :: that2
    character((that2%l1+1) * that2%l2 + 3) :: locstring
    integer :: i
    locstring = ""
    write(locstring,"(999a)") "3a:", (that2%charr(i), ".", i=1,that2%l2)
    call that % p2(dt(len(locstring))(locstring,that%ival,this%p1,this%p2,this%p3))
  end subroutine disp3a

end module dtpPPCLocalPassDTPInterfaceLSubmod


program dtpPPCLocalPassDTPInterfaceLSub

  use dtpPPCLocalPassDTPInterfaceLSubmod
  implicit none
  type(dt(0)) :: t0a, t0b
  type(dt(4)) :: t4a, t4b
  type(dt(3)) :: t3c, t3d
  type(dt(:)) :: tp
  type(dt2(1,2,3)) :: t2_a
  type(dt2(1,1,4)) :: t2_b
  type(dt2(1,:,:)) :: t2_p
  target  :: t3c, t2_a, t2_b
  pointer :: tp, t2_p

  t0a = dt(0)("",127,disp1,disp2,disp3)
  t0b = dt(0)("",-127,disp1a,disp2a,disp3a)
  t4a = dt(4)("abcd",32000,disp1,disp2,disp3)
  t4b = dt(4)("efgh",-12345,disp1a,disp2a,disp3a)
  t3c = dt(3)("ijk",1,disp1,disp2,disp3)
  t3d = dt(3)("lmn",-1,disp1a,disp2a,disp3a)
  t2_a = dt2(1,2,3)(["op","qr","st"])
  t2_b = dt2(1,1,4)(["u","v","w","x"])
  tp   => t3c
  t2_p => t2_b


  call t0a % p1
  call t0b % p1
  call t3c % p1
  call t3d % p1
  call tp  % p1
  call t4a % p1
  call t4b % p1
  print *

  call t0a % p2(t0a)
  call t0a % p2(t0b)
  call t0a % p2(t3c)
  call t0a % p2(tp)
  call t3c % p2(tp)
  call tp  % p2(tp)
  call t4a % p2(tp)
  print *

  call t0b % p2(t0a)
  call t0b % p2(t0b)
  call t0b % p2(t3d)
  call t3d % p2(tp)
  call t4b % p2(tp)
  print *

  call tp % p3(t0a,t2_p)
  call tp % p3(t0b,t2_p)
  call tp % p3(t3c,t2_p)
  call tp % p3(t3d,t2_p)
  call tp % p3(tp,t2_p)
  call tp % p3(t4a,t2_p)
  call tp % p3(t4b,t2_p)
  print *

  call t4b % p3(t0a,t2_a)
  call t4b % p3(t0b,t2_a)
  call t4b % p3(t3c,t2_a)
  call t4b % p3(t3d,t2_a)
  call t4b % p3(tp,t2_a)
  call t4b % p3(t4a,t2_a)
  call t4b % p3(t4b,t2_a)
  print *


  t0a % p1 => disp1a
  t0a % p2 => disp2a
  t0a % p3 => disp3a

  t0b % p1 => disp1
  t0b % p2 => disp2
  t0b % p3 => disp3

  t3c % p1 => disp1a
  t3c % p2 => disp2a
  t3c % p3 => disp3a

  t3d % p1 => disp1
  t3d % p2 => disp2
  t3d % p3 => disp3

  t4a % p1 => disp1a
  t4a % p2 => disp2a
  t4a % p3 => disp3a

  t4b % p1 => disp1
  t4b % p2 => disp2
  t4b % p3 => disp3

  allocate(tp, source=dt(5)("opqrs",0,disp1a,disp2a,disp3a))

  call t0a % p1
  call t0b % p1
  call t3c % p1
  call t3d % p1
  call tp  % p1
  call t4a % p1
  call t4b % p1
  print *

  call t0a % p2(t0a)
  call t0a % p2(t0b)
  call t0a % p2(t3c)
  call t0a % p2(tp)
  call t3c % p2(tp)
  call tp  % p2(tp)
  call t4a % p2(tp)
  print *

  call t0b % p2(t0a)
  call t0b % p2(t0b)
  call t0b % p2(t3d)
  call t3d % p2(tp)
  call t4b % p2(tp)
  print *

  call tp % p3(t0a,t2_p)
  call tp % p3(t0b,t2_p)
  call tp % p3(t3c,t2_p)
  call tp % p3(t3d,t2_p)
  call tp % p3(tp,t2_p)
  call tp % p3(t4a,t2_p)
  call tp % p3(t4b,t2_p)
  print *

  call t4b % p3(t0a,t2_a)
  call t4b % p3(t0b,t2_a)
  call t4b % p3(t3c,t2_a)
  call t4b % p3(t3d,t2_a)
  call t4b % p3(tp,t2_a)
  call t4b % p3(t4a,t2_a)
  call t4b % p3(t4b,t2_a)
  print *



  allocate(tp, source=dt(5)("opqrs",123454321,disp1,disp2,disp3))
  call tp % p1
  call tp % p2(tp)
  call tp % p3(dt(3)("tuv", 543212345, disp1a),dt2(1,0,0)([character(0)::]))

  print *, "done"

end program dtpPPCLocalPassDTPInterfaceLSub
