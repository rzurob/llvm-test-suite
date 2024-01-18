!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyInLen
!*  TEST CASE FILE             : dtpCompatDummyInLenPostDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code calls subroutine, passing in reference to extended type with LEN type parameter
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
!*  See description in dtpCompatDummyInLenPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyInLenPostDTPmod
  use :: dtpCompatDummyInLenPreDTPmod
  type, extends(base) :: child (l)
     integer, len  :: l
     character(l) :: chval
     integer(1) :: ival(l)
     integer(1) :: ival2
     character(l) :: chval2
  end type child
end module dtpCompatDummyInLenPostDTPmod

program dtpCompatDummyInLenPostDTP
  use :: dtpCompatDummyInLenPostDTPmod
  class(base), allocatable, target :: b, c, d

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyInLenCopy(b)
  call test("p #1", p)
  print *, "allocating child"
  allocate(c, source=child(3)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local copy; no failure expected"
  allocate(d, source=c)
  call test("d", d)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatDummyInLenCopy(c)
  print *, "End. If you can see this, then the test failed."
  call test("p #2", p)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(*)); print *, txt, " - child(*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (base);       print *, txt, " - base: ", a%i
    class default;        print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyInLenPostDTP
