! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (array variable)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer(4)   :: i = 9
   end type

   type, extends(base), abstract ::  child
      real(4)      :: r = 9.0
   end type

   type, extends(child) :: gen3
      character(3) :: c = 'xxx'
   end type

end module

program input105
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(base)               :: b1(2)
   type(base), pointer      :: b2(:)
   type(gen3)               :: b3(2,2)
   type(gen3), allocatable  :: b4(:,:)

   namelist /n1/ b1, b2
   namelist /n1/ b3, b4

   allocate(b2(3),b4(3,3))

   open (1, file='input105.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   print *, b1
   print *, b2
   print *, b3
   print *, b4

end program
