! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass018.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2005
!*
!*  DESCRIPTION                : class keyword (specification expr in array
!                               declaration)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child    ! (4)
   end type

   interface operator(+)
      function myAdd1(a,b)
         import base, child
         type(base(4)), intent(in), dimension(:) :: a, b
         type(base(4)) :: myAdd1 (size(a))
      end function
   end interface

end module

program fclass018
   use m

   class(base(4)), dimension(:), allocatable :: c1, c2, c3, c4

   allocate(c1(2), source=(/ base(4)(3), base(4)(3) /) )
   allocate(c2(2), source=(/ base(4)(1), base(4)(1) /)  )

   allocate(c3(2), source=(c1+c2))

    if (any(c3%id /= (/4,4/))) error stop 1_4
end program


function myAdd1(a,b)
   use m, only: base, child
   type(base(4)), intent(in), dimension(:) :: a, b
   type(base(4)):: myAdd1(size(a))

   if ( size(a) .eq. size(b) ) then
      myAdd1%id = a%id + b%id
   else
      error stop 3_4
   end if
end function

