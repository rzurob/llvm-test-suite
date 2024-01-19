!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatListComponent2PreDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : use of derived type components with polymorphic references (list includes DTP object)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatListComponent (<-dtpCompatDummyInGrandChild<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  Here we test "old" module subroutines which examine or copy lists of objects of an extended class;
!*  the "post" code for this case extends the list class with a DTP child (KIND).
!*  The subroutine itemCount merely uses the reference as a pointer, and should trigger no error.
!*  The copy routine only copies the first element, and should not encounter problems unless the
!*  first element can cause them.  The routine "deepishCopy" attempts to copy more content, but
!*  will generate no error, even if either the "next" or the "datum" pointer reference DTP objects,
!*  as long as those objects contain no or only KIND type parameters, as in this case.
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatListComponent2PreDTPmod

  type base
     integer :: i
  end type base

  type list
     integer :: id
     class(base), pointer :: datum => null()
     class(list), pointer :: next => null()
  end type list

contains

  recursive integer function itemCount(arg)
    class(list), pointer, intent(in) :: arg
    if (associated(arg)) then
       itemCount = itemCount(arg%next) + 1
    else
       itemCount = 0
    end if
  end function itemCount

  subroutine copy(sink, src)
    class(list), pointer, intent(in) :: src
    class(list), pointer, intent(out) :: sink
    allocate(sink, source=src)
  end subroutine copy

  recursive subroutine deepishCopy(sink, src)
    class(list), pointer, intent(in) :: src
    class(list), pointer, intent(out) :: sink
    if (associated(src)) then
       allocate(sink, source=src)
       allocate(sink%datum, source=src%datum)
       call deepishCopy(sink%next, src%next)
    end if
  end subroutine deepishCopy

end module dtpCompatListComponent2PreDTPmod
