!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : pointer to external PASS function (with intrinsic return) from DTP with K+L param with other args of mixed types
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalPassMixInterfaceKLSub (<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to functions (returning intrinsic
!*  types) which expect a passed-object dummy argument and other args of mixed types.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  References objects via local variables.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalPassMixInterfaceKLFunIntrExtmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (s1), pointer, pass     :: p1 => null()
     procedure (s1), pointer, pass(a2) :: p2 => null()
     procedure (s1), pointer, pass(a4) :: p4 => null()
     procedure (s5), pointer, pass     :: pR => null()
     procedure (s6), pointer, pass     :: pCI => null()
  end type dt

  abstract interface

     character(100) function s1(a1,a2,a4)
       import :: dt
       class(dt(1,*)), intent(in) :: a1
       class(dt(2,*)), intent(in) :: a2
       class(dt(4,*)), intent(in) :: a4
     end function

     real function s5(this,r1)
       import :: dt
       class(dt(2,*)), intent(in) :: this
       real(4), intent(in) :: r1
     end function s5

     integer(8) function s6(this,ch,i1)
       import :: dt
       class(dt(4,*)), intent(in) :: this
       character(*), intent(in)   :: ch
       integer(4), intent(in)     :: i1
     end function s6

  end interface

end module dtpPPCLocalPassMixInterfaceKLFunIntrExtmod


program dtpPPCLocalPassMixInterfaceKLFunIntrExt

  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  implicit none
  type(dt(1,0)) :: t1a, t1b
  type(dt(2,4)) :: t2a, t2b
  type(dt(1,3)) :: t1c, t1d
  type(dt(1,:)) :: t1p
  type(dt(4,:)) :: t4ap, t4bp
  target  :: t1c, t1d
  pointer :: t1p, t4ap, t4bp

  integer(8)     :: iresult
  real           :: rresult
  character(100) :: chresult

  interface
     character(100) function fun1(this,that,that2)
       import :: dt
       class(dt(1,*)), intent(in) :: this
       class(dt(2,*)), intent(in) :: that
       class(dt(4,*)), intent(in) :: that2
     end function fun1
     character(100) function fun1a(this,that,that2)
       import :: dt
       class(dt(1,*)), intent(in) :: this
       class(dt(2,*)), intent(in) :: that
       class(dt(4,*)), intent(in) :: that2
     end function fun1a
     real function funR(this,r1)
       import :: dt
       class(dt(2,*)), intent(in) :: this
       real(4), intent(in) :: r1
     end function funR
     real function funRa(this,r1)
       import :: dt
       class(dt(2,*)), intent(in) :: this
       real(4), intent(in) :: r1
     end function funRa
     integer(8) function funCI(this,ch,i1)
       import :: dt
       class(dt(4,*)), intent(in) :: this
       character(*), intent(in)   :: ch
       integer(4), intent(in)     :: i1
     end function funCI
     integer(8) function funCIa(this,ch,i1)
       import :: dt
       class(dt(4,*)), intent(in) :: this
       character(*), intent(in)   :: ch
       integer(4), intent(in)     :: i1
     end function funCIa
  end interface

  t1a = dt(1,0)("",127,fun1,fun1,fun1,funR,funCI)
  t1b = dt(1,0)("",-127,fun1a,fun1a,fun1a,funRa,funCI)
  t2a = dt(2,4)("abcd",32000,fun1,fun1,fun1,funR,funCI)
  t2b = dt(2,4)("efgh",-12345,fun1a,fun1a,fun1a,funRa,funCI)
  t1c = dt(1,3)("ijk",1,fun1,fun1,fun1,funR,funCI)
  t1d = dt(1,3)("lmn",-1,fun1a,fun1a,fun1a,funRa,funCI)
  t1p => t1c
  allocate(t4ap, source=dt(4,5)("opqrs",123454321,fun1,fun1,fun1,funR,funCI))
  allocate(t4bp, source=dt(4,5)("tuvwx",123454321,fun1a,fun1a,fun1a,funRa,funCIa))

  print *, t1a % p1(t2a, t4ap)
  print *, t2a % p2(t1a, t4ap)
  print *, t4ap % p4(t1a, t2a)
  print *, t2a % pR(5.1)
  print *, t4ap % pCI("tuvw", 123454321)
  print *

  print *, t1b % p1(t2a, t4ap)
  print *, t2b % p2(t1a, t4ap)
  print *, t4bp % p4(t1a, t2a)
  print *, t2b % pR(5.1)
  print *, t4bp % pCI("xyzab", 123454321)
  print *

  print *, t1p % p1(t2a, t4ap)
  print *, t2a % p2(t1p, t4ap)
  print *, t4ap % p4(t1p, t2a)
  print *

  print *, t1d % p1(t2b, t4bp)
  print *, t2a % p2(t1d, t4bp)
  print *, t4ap % p4(t1d, t2b)
  print *

  print *, t2b % p2(t1d, t4bp)
  print *, t4bp % p4(t1d, t2b)
  print *

  ! Change some of the pointers (but not all - we want to be sure the right ones are invoked)
  t1a % p1 => fun1a
  t1a % p4 => fun1a
  t1a % pR => funRa
  t1a % pCI=> funCIa

  t1b % p4 => fun1

  t1c % p1 => fun1a
  t1c % p1 => fun1a
  t1c % p1 => fun1a
  t1c % pR => funRa

  t1d % p1 => fun1
  t1d % pCI=> funCI

  t2a % p2 => fun1a
  t2a % pR => funRa

  t2b % p4 => fun1
  t2b % pR => funR

  t4ap%p1 => t4bp%p1
  t4ap%p2 => t4bp%p2

  t4bp%p2 => fun1
  t4bp%p4 => t4ap%p4
  t4bp%pCI=> t4ap%pCI

  t1p => t1d

  chresult = t1a % p1(t2a, t4ap)
  print *, chresult
  chresult = t2a % p2(t1a, t4ap)
  print *, chresult
  chresult = t4ap % p4(t1a, t2a)
  print *, chresult
  rresult  = t2a % pR(5.1)
  print *, rresult
  iresult  = t4ap % pCI("cd", 123454321)
  print *, iresult
  print *

  chresult = t1b % p1(t2a, t4ap)
  print *, chresult
  chresult = t2b % p2(t1a, t4ap)
  print *, chresult
  chresult = t4bp % p4(t1a, t2a)
  print *, chresult
  rresult  = t2b % pR(5.1)
  print *, rresult
  iresult  = t4bp % pCI("efghijkl", 123454321)
  print *, iresult
  print *

  chresult = t1c % p1(t2a, t4ap)
  print *, chresult
  chresult = t2a % p2(t1c, t4ap)
  print *, chresult
  chresult = t4ap % p4(t1c, t2a)
  print *, chresult
  print *

  chresult = t1d % p1(t2b, t4bp)
  print *, chresult
  chresult = t2a % p2(t1d, t4bp)
  print *, chresult
  chresult = t4ap % p4(t1d, t2b)
  print *, chresult
  print *

  chresult = t2b % p2(t1d, t4bp)
  print *, chresult
  chresult = t4bp % p4(t1d, t2b)
  print *, chresult
  print *


  chresult = t4ap % p4(dt(1,4)("WXYZ",11111111,fun1,fun1a,fun1,funR,funCIa), &
                       dt(2,2)("AB",  22222222,fun1a,fun1,fun1a,funRa,funCI))
  print *, chresult

  print *, "done"

end program dtpPPCLocalPassMixInterfaceKLFunIntrExt



character(100) function fun1(this,that,that2)
  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  class(dt(1,*)), intent(in) :: this
  class(dt(2,*)), intent(in) :: that
  class(dt(4,*)), intent(in) :: that2
  write(fun1,*) "in fun1:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<", &
           that%ival, kind(that%ival), that%k, len(that%chval), that%l, ">", that%chval, "<", &
           that2%ival, kind(that2%ival), that2%k, len(that2%chval), that2%l, ">", that2%chval, "<"
end function fun1

character(100) function fun1a(this,that,that2)
  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  class(dt(1,*)), intent(in) :: this
  class(dt(2,*)), intent(in) :: that
  class(dt(4,*)), intent(in) :: that2
  write(fun1a,*) "in fun1a:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<", &
           that%ival, kind(that%ival), that%k, len(that%chval), that%l, ">", that%chval, "<", &
           that2%ival, kind(that2%ival), that2%k, len(that2%chval), that2%l, ">", that2%chval, "<"
end function fun1a

real function funR(this,r1)
  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  class(dt(2,*)), intent(in) :: this
  real(4), intent(in) :: r1
  funR = abs(r1 * this%ival * len(this%chval))
end function funR


real function funRa(this,r1)
  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  class(dt(2,*)), intent(in) :: this
  real(4), intent(in) :: r1
  funRa = -abs(r1 * this%ival * len(this%chval))
end function funRa


integer(8) function funCI(this,ch,i1)
  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  class(dt(4,*)), intent(in) :: this
  character(*), intent(in)   :: ch
  integer(4), intent(in)     :: i1
  funCI = 1000000000000_8*i1 + 100000000000_8*len(ch) + 10000000000_8*len(this%chval) + this%ival
end function funCI


integer(8) function funCIa(this,ch,i1)
  use :: dtpPPCLocalPassMixInterfaceKLFunIntrExtmod
  class(dt(4,*)), intent(in) :: this
  character(*), intent(in)   :: ch
  integer(4), intent(in)     :: i1
  funCIa = 1000000000000_8*i1 + 100000000000_8*len(ch) + 10000000000_8*len(this%chval) + 1000000000_8 + this%ival
end function funCIa
