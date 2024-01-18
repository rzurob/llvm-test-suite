!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyInKind
!*  TEST CASE FILE             : dtpCompatDummyInKindPostDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code calls subroutine, passing in reference to extended type with KIND type parameter
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpCompatDummyInKindLen (<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyInKindPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyInKindPostDTPmod
  use :: dtpCompatDummyInKindPreDTPmod
  type, extends(base) :: child (k)
     integer, kind :: k
     character(3) :: chval
     integer(k) :: ival(3)
     integer(k) :: ival2
     character(3) :: chval2
  end type child
end module dtpCompatDummyInKindPostDTPmod

program dtpCompatDummyInKindPostDTP
  use :: dtpCompatDummyInKindPostDTPmod
  class(base), allocatable, target :: b, c, d

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyInKindCopy(b)
  call test("p #1", p)
  print *, "allocating child"
  allocate(c, source=child(1)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local copy; no failure expected"
  allocate(d, source=c)
  call test("d", d)
  print *, "calling old implementation to copy; expecting no runtime error message"
  call dtpCompatDummyInKindCopy(c)
  call test("p #2", p)
  print *, "End."

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(1)); print *, txt, " - child(1):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (child(2)); print *, txt, " - child(2):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (child(4)); print *, txt, " - child(4):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (base);       print *, txt, " - base: ", a%i
    class default;        print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyInKindPostDTP
