  module m

      type A
          complex(4), allocatable :: c4(:)
          complex(8), allocatable :: c8(:)
          complex(16), allocatable :: c16(:)
      end type

      type, extends(A) :: B
	  class(*), pointer :: ptr(:)
      end type

  end module

