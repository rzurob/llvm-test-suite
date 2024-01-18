!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatListComponent
!*  TEST CASE FILE             : dtpCompatListComponentPostDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : use of derived type components with polymorphic references
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpCompatDummyInGrandChild (<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatListComponentPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatListComponentPostDTPmod

  use :: dtpCompatListComponentPreDTPmod

  type, extends(base) :: child (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l) :: chval
     integer(k) :: ival(l)
     integer(k) :: ival2
     character(l) :: chval2
  end type child

end module dtpCompatListComponentPostDTPmod

program dtpCompatListComponentPostDTP
  use :: dtpCompatListComponentPostDTPmod
  class(list), pointer :: p, p2
  class(base), pointer :: bp1, bp2, bp3, bp4

  allocate (bp1, source=base(1))
  allocate (bp2, source=base(2))
  allocate (bp3, source=child(1,3)(3,"abc",[123_1,45_1,67_1],89_1,"def"))
  allocate (bp4, source=base(4))

  allocate (p, source=list(101,bp1))
  allocate (p%next, source=list(102,bp2))

  call printList("p 1", p)
  if (itemCount(p) /= 2) stop 2

  if (p%next%id /= 102) stop 3
  call copy(p2, p)
  p2%next%id = 44
  call printList("p2 1", p2)
  if (p%next%id /= 44 .or. itemCount(p2) /= 2) stop 4
  deallocate(p2)

  call deepishCopy(p2, p)
  p2%next%id = 55
  call printList("p2 2", p2)
  if (p%next%id /= 44 .or. itemCount(p2) /= 2) stop 5

  allocate (p%next%next, source=list(103,bp3))
  allocate (p%next%next%next, source=list(104,bp4))

  if (itemCount(p) /= 4) stop 6

  ! yes, there's a memory leak, but we're almost done...
  call deepishCopy(p2, p)
  print *, "If you can read this, the test failed."
  p%next%next%next%id = 66
  call printList("p 2", p)
  call printList("p2 3", p2)

  print *, itemCount(p2)
  stop 7

contains

  subroutine printList(txt,l)
    character(*) :: txt
    class(list), pointer, intent(in) :: l
    class(list), pointer :: lp
    lp => l
    print *, txt
    do while (associated(lp))
       if (.not.associated(lp%datum)) then
          print *, " ", lp%id, " - null"
       else
          select type (a => lp%datum)
          type is (child(1,*)); print *, " ", lp%id, " - child(1,*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
          type is (child(2,*)); print *, " ", lp%id, " - child(1,*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
          type is (child(4,*)); print *, " ", lp%id, " - child(1,*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
          type is (base);       print *, " ", lp%id, " - base:", a%i
          class default;        print *, " ", lp%id, " - unknown type"
          end select
       end if
       lp => lp % next
    end do
  end subroutine printList

end program dtpCompatListComponentPostDTP
