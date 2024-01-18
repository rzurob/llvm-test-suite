!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d360954.f
!*
!*  DATE                       : Jan. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
     integer,len   :: l1
  end type
  type base(l2)
     integer,len  :: l2
     type(A(4)) :: a1
  end type

  contains
    subroutine printA(at)
      class(A(*)),intent(inout) :: at
      select type(at)
        type is(A(*))
             print *,at%l1
        class default
           stop 2
      end select
    end subroutine

    subroutine printBase(bt)
       class(base(*)),intent(inout) :: bt
       select type(bt)
          type is(base(*))
             print *,bt%a1%l1
             call printA(bt%a1)
          class default
             stop 1
       end select
    end subroutine

end module

program d360954
  use m

  type(base(3)),allocatable :: bt
  allocate(base(3) :: bt)

  call printBase(bt)

end program
