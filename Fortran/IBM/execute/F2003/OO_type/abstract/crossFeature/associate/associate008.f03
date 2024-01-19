! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Associate Construct
!*                               e) Associate-name associating with type components
!*                                  1) multiple level of associate construct abstract polymorphic array section as selector
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type :: data
      integer :: i
   contains
      procedure, pass :: getid
   end type

   type, abstract :: base
      type(data) :: id
   contains
      procedure, nopass :: type => basetype
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: type => childtype
   end type

contains

   integer function basetype()
      basetype = 1
   end function

   integer function childtype()
      childtype = 2
   end function

   integer elemental function getid(a)
      class(data), intent(in) :: a
      getid = a%i
   end function

end module

program associate008
   use m

   class(base), allocatable :: b1(:)
   allocate (b1(3), source = (/ child(data(5)), child(data(6)), child(data(7)) /) )

   associate ( myb1 => b1(1:3:2)%id, myb2 => b1(2:3:2)%id, myb3 => b1(3:1:-2)%id, myb4 => b1((/1,2,3/))%id )

      print *,myb1%getid()
      print *,myb2%getid()
      print *,myb3%getid()
      print *,myb4%getid()

      associate ( myb11 => myb1%i )
         print *, myb11
      end associate
   end associate

   print *, b1%id%i

end program

