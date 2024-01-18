!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d360963.f
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
  type A(k1,l1)
     integer,kind  :: k1
     integer,len   :: l1
  end type
  type base(l2)
     integer,len  :: l2
     type(A(2,4)) :: a1
  end type

  contains

    subroutine printA(at)
      class(A(2,*)),intent(inout) :: at
      print *,at%k1,at%l1
      select type(at)
        type is(A(2,*))
            print *,at%k1,at%l1
        class default
           stop 2
      end select
    end subroutine

    subroutine printBase(bt)
       class(base(*)),intent(inout) ::  bt
       select type(bt)
          type is(base(*))
             print *,bt%a1%k1,bt%a1%l1
             call printA(bt%a1)
          class default
             stop 1
       end select
    end subroutine

end module

program d360963
  use m

  class(base(3)),allocatable :: bt

  allocate(base(3) :: bt)

  call printBase(bt)

end program
