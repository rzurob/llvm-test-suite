! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/userename/userenamenest002.f
! opt variations: -qnol -qnodeferredlp -qreuse=self

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : rename a public operator binary nested module
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj(n1,k1,k2)    ! (20,4,4)
      integer, kind :: k1,k2
      integer, len  :: n1
      integer(k1)   :: x = -999
      integer(k2)   :: y = -999
      contains
         procedure :: mybadd
         generic :: operator(.add.) => mybadd
   end type

   interface operator(.add.)
   		module procedure mybadd2
   end interface

   contains

   function mybadd ( a, b )
      class(baseobj(*,4,4)), intent(in) :: a
      type(baseobj(*,4,4)), intent(in)  :: b

      type(baseobj(20,4,4)) :: mybadd

      mybadd%x = a%x + b%x
      mybadd%y = a%y + b%y

      print *, 'mybadd'

   end function

   function mybadd2 ( a, b )
      integer, intent(in) :: a
      integer, intent(in)  :: b

      integer :: mybadd2

      mybadd2 = a + b + 1000


      print *, 'mybadd2'

   end function



end module
module m2
use m, operator (.plus.) => operator(.add.)
end module

program mainprog
   use m2

   type(baseobj(20,4,4)), target      :: b1
   type(baseobj(:,4,4)), pointer     :: b2
   type(baseobj(:,4,4)), allocatable :: b3
   integer :: i1=5,i2=5,i3=0
   b1 = baseobj(20,4,4) ( 100, 200 )

   allocate ( b2, source = baseobj(20,4,4) ( 101, 201 ) )
   allocate ( b3, source = baseobj(20,4,4) () )

   b3 = b1 .add. b2
   print *, b3%x

   i3 = i1 .plus. i2
   print *, i3

end program