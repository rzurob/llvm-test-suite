!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCModPassNArgInterfaceLFunDTP
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to pass function with no other dummy args, DTP(L) result, ref via module var
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCModNPassDTPInterfaceKLFunDTP (<-dtpPPCLocalNPassDTPInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunIntr<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to functions which return a
!*  value of derived type and only expect a passed-object dummy argument.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses module variables to reference objects.  The arg which is passed in has
!*  only a len parameter.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCModPassNArgInterfaceLFunDTPmod

  implicit none

  type dt (l)
     integer, len  :: l
     character(l)  :: chval
     integer(2)    :: ival
     character(l)  :: chval2
     procedure (f1), pointer, pass :: p1 => null()
  end type dt

  abstract interface

     type(dt(40)) function f1(a1)
       import :: dt
       class(dt(*)), intent(in) :: a1
     end function f1

  end interface


  type(dt(1)) :: t1
  type(dt(2)) :: t2
  type(dt(3)) :: t3
  type(dt(:)) :: tp

  target  :: t1, t2, t3
  pointer :: tp

  save :: t1, t2, t3


contains


  type(dt(40)) function swap1(this)
    class(dt(*)), intent(in) :: this
    swap1%chval  = this%chval2
    swap1%ival   = this%ival
    swap1%chval2 = this%chval
  end function swap1

  type(dt(40)) function swap1a(this)
    class(dt(*)), intent(in) :: this
    character(40) :: c40
    character(39) :: c39
    character(1)  :: c1(40)
    equivalence (c1(1),c40), (c1(2),c39)
    c1(1) = 'a'
    c39   = this%chval2
    swap1a%chval  = c40 ! long way to say "swap1a%chval = 'a' // this%chval2"
    swap1a%ival   = this%ival
    c1(1) = 'b'
    c39   = this%chval
    swap1a%chval2 = c40
  end function swap1a

  subroutine display(this)
    class(*), intent(in) :: this
    select type (this)
    type is (dt(*)); print *, this%ival, this%l, ">", trim(this%chval) // trim(this%chval2), "<"
    class default;   print *, "Unkown type"
                     stop 2
    end select
  end subroutine display

end module dtpPPCModPassNArgInterfaceLFunDTPmod


program dtpPPCModPassNArgInterfaceLFunDTP

  use dtpPPCModPassNArgInterfaceLFunDTPmod
  implicit none
  type(dt(40)):: t1res

  t1 = dt(1)("x",127,"y",swap1)
  t2 = dt(2)("ab",32000,"cd",swap1a)
  t3 = dt(3)("efg",-12345,"hij",swap1)
  tp => t1

  call display(t1 % p1())
  call display(t2 % p1())
  call display(t3 % p1())
  call display(tp % p1())
  tp => t2
  call display(tp % p1())
  tp => t3
  call display(tp % p1())
  print *

  t1 % p1 => swap1a
  t2 % p1 => swap1
  t3 % p1 => swap1a
  allocate(tp, source=dt(5)("opqrs",4210,"tuvwx",swap1))

  call display(t1 % p1())
  call display(t2 % p1())
  call display(t3 % p1())
  call display(tp % p1())
  print *

  print *, "done"

end program dtpPPCModPassNArgInterfaceLFunDTP
