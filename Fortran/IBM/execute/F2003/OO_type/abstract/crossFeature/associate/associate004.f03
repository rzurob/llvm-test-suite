! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Associate Construct
!*                                         b) Associate-name associating with array variable(s) (array section with vector subscripts)
!*                                            1) variable being an abstract type object (pointer)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base
      integer :: id
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
end module

program associate004
   use m
   class(base), pointer :: b1(:)
   class(base), allocatable, target :: b2(:)

   allocate (b2(3), source = (/child(8),child(10),child(9)/) )

   b1 => b2

   associate ( myb1 => b1((/1,3/)), myb2 => b2((/3,2/)) )
      if ( myb1%type() .ne. 2 )         error stop 1_4
      if ( myb2%type() .ne. 2 )         error stop 2_4
      print *, myb1%id
      print *, myb2%id
   end associate

end program
