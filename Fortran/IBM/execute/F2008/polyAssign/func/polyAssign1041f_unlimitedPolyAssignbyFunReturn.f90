! *********************************************************************
!* ===================================================================
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* DATE                         : 05 August 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!* SECONDARY FUNTIONS TESTED    : LHS of intrinsic assignment with function return
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!* Test LHS of intrinsic assignment with function return.
!* LHS variable is unlimited polymorphic entity.
!* RHS expression is a function returning an allocatable object.
!* Change the type of argument inside the function.
!* Re-assign LHS variable with other type which is different with LHS.
!* Define array whose element is an allocatable unlimited type.
!* Re-assign array element with other defined type.
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
    integer:: j=4
    integer:: k=5
  end type t2

  type(t) :: arr(5)
  class(*), target, allocatable :: x1
  class(*), pointer :: p
  integer :: i
  do i=1,5
    x1 = retPoly(p)
    arr(i)%x = t2(i,i+1)
    x1 = arr(i)%x
    p=>x1
    if ( same_type_as(arr(i)%x, x1) .neqv. .true. ) error stop 1
    select type (p)
      type is (t2)
        print *, "inside main, type of p is t2, p%j==", p%j, "p%k==", p%k
      type is (t)
        print *, "inside main, type of p is t"
      class default
        print *, "not related type in main"
    end select
    print *, '**************','end of loop for i=', i, '*************'
    print *, ''
  end do

  contains
  function retPoly(p)
    class(*), pointer :: p
    class(t), allocatable :: retPoly
    select type (p)
      type is (t2)
        print *, "inside retPoly, type of p is t2, p%j==", p%j, "p%k==", p%k
        retPoly = t(null)
      type is (t)
        print *, "inside main, type of p is t"
      class default
        retPoly = t(null)
        print *, "not related type in retPoly"
    end select
  end function
end
