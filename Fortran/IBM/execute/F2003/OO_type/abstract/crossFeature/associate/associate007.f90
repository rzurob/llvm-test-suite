!######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate007.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
!*                                  1) multiple level of associate construct abstract polymorphic array as selector
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

   integer function getid(a)
      class(data), intent(in) :: a
      getid = a%i
   end function
end module

program associate007
   use m

   class(base), allocatable, target :: b1(:)
   allocate (b1(3), source = (/ child(data(5)), child(data(6)), child(data(7)) /) )

   associate (myb1 => b1%id )

      if ( size(myb1) .ne. size(b1%id) ) error stop 1_4
      if ( myb1(1)%getid() .ne. 5 ) error stop 2_4
      if ( myb1(2)%getid() .ne. 6 ) error stop 3_4
      if ( myb1(3)%getid() .ne. 7 ) error stop 4_4
      associate ( myb11 => myb1%i )
         if ( size(myb11) .ne. size(myb1%i) ) error stop 5_4
         if ( myb11(1) .ne. 5 ) error stop 6_4
         if ( myb11(2) .ne. 6 ) error stop 7_4
         if ( myb11(3) .ne. 7 ) error stop 8_4
      end associate

   end associate

end program

