!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatListComponent2
!*  TEST CASE FILE             : dtpCompatListComponent2PostDTP
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
!*  See description in dtpCompatListComponent2PreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatListComponent2PostDTPmod

  use :: dtpCompatListComponent2PreDTPmod

  type, extends(list) :: list2 (k)
     integer, kind :: k
     integer(k) :: id2
  end type list2

  type, extends(base) :: child
     character(3) :: chval
     integer(1) :: ival(3)
     integer(1) :: ival2
     character(3) :: chval2
  end type child

end module dtpCompatListComponent2PostDTPmod

program dtpCompatListComponent2PostDTP
  use :: dtpCompatListComponent2PostDTPmod
  class(list), pointer :: p, p2
  class(base), pointer :: bp1, bp2, bp3, bp4

  allocate (bp1, source=base(1))
  allocate (bp2, source=base(2))
  allocate (bp3, source=child(3,"abc",[123_1,45_1,67_1],89_1,"def"))
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

  allocate (p%next%next, source=list2(4)(103,bp3,null(),203))
  allocate (p%next%next%next, source=list(104,bp4))

  if (itemCount(p) /= 4) stop 6

  ! yes, there's a memory leak, but we're almost done...
  print *, "deepishCopy: calling old implementation to copy; expecting no runtime error message"
  call deepishCopy(p2, p)
  p%next%next%next%id = 66
  call printList("p 2", p)
  call printList("p2 3", p2)

  print *, itemCount(p2)
  print *, "End."

contains

  subroutine printList(txt,l)
    character(*) :: txt
    class(list), pointer, intent(in) :: l
    class(list), pointer :: lp
    integer :: listType
    lp => l
    print *, txt
    do while (associated(lp))
       select type (a => lp)
       type is (list2(1)); listType = 1
       type is (list2(2)); listType = 2
       type is (list2(4)); listType = 4
       type is (list);     listType = 0
       class default;      listType = -666
       end select
       if (.not.associated(lp%datum)) then
          print *, " list", listType, lp%id, " - null"
       else
          select type (a => lp%datum)
          type is (child); print *, " list", listType, lp%id, " - child:", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
          type is (base);  print *, " list", listType, lp%id, " - base:", a%i
          class default;   print *, " list", listType, lp%id, " - unknown type"
          end select
       end if
       lp => lp % next
    end do
  end subroutine printList

end program dtpCompatListComponent2PostDTP
