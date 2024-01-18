! *********************************************************************
!* ===================================================================
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* DATE                         : 05 August 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : LHS is an unlimited polymorphic scalar, RHS is an object of a derived type which is also an element of an array.
!*                              : Test LHS of intrinsic assignment with attributes of unlimited polymorphic, target, pointer and allocatable.
!*                              : LHS variable is unlimited polymorphic entity, RHS is an object of a derived type.
!*                              : Define array whose elements are allocatable unlimited type
!*                              : Re-assign array element with other defined type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/05/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
  type t
    class(*), allocatable:: x
  end type t

  type :: t2
    integer:: j=40
    integer:: k=50
  end type t2

  type(t) :: arr(4)
  class(*), target, allocatable :: x1
  class(*), pointer :: p
  integer :: i
  !do i=1,10,1
  do i= lbound(arr, 1), ubound(arr, 1)
    arr(i)%x = t2(i,i+1)
    print *, "after 'arr(i)%x = t2(i)', allocated (arr(i)%x)==", allocated (arr(i)%x)
    x1 = arr(i)%x
    print *, "after 'x1 = arr(i)%x' ,storage_size(x1)==", storage_size(x1)
    p=>x1
    print *, 'after p=>x1, same_type_as(arr(i)%x, x1)==',same_type_as(arr(i)%x, x1)
    select type (p)
      type is (t2)
        print *, "type of p is t2, p%j==", p%j, " p%k==", p%k
      type is (t)
        print *, "type of p is t"
      class default
        print *, "not related type in main"
    end select
    print *, '**************','end of loop for i=', i, '*************'
    print *, ''
  end do
end
