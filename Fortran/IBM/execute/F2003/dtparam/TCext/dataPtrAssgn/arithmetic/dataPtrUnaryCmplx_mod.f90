! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrUnaryCmplx_mod.f
! opt variations: -qnol

  module m

      type A(n1,k1,k2,k3)    ! (20,4,8,16) 
          integer, kind            :: k1,k2,k3
          integer, len             :: n1
          complex(k1), allocatable :: c4(:)
          complex(k2), allocatable :: c8(:)
          complex(k3), allocatable :: c16(:)
      end type

      type, extends(A) :: B    ! (20,4,8,16)
	  class(*), pointer :: ptr(:)
      end type

  end module
