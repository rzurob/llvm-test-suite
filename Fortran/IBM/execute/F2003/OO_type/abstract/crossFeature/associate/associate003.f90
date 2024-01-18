!######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate003.f
! %VERIFY: associate003.out:associate003.vf
! %STDIN:
! %STDOUT: associate003.out
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
!*                                         b) Associate-name associating with array variable(s) (array section)
!*                                            1) variable being an abstract type array section
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
      procedure, pass :: getid => baseid
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

   integer elemental function baseid(a)
      class(base), intent(in) :: a
      baseid = a%id
   end function

end module

program associate003
   use m

   class(base), allocatable :: b1(:)
   class(base), pointer :: b2(:)
   allocate (b2(3), source = (/child(2),child(3),child(1)/) )
   allocate (b1(3), source = (/child(8),child(10),child(9)/) )

   associate ( myb1 => b1(1:3:2), myb2 => b2(3:1:-2) )
      if ( myb1%type() .ne. 2 )         error stop 1_4
      if ( myb2%type() .ne. 2 )         error stop 2_4

      print *, myb1%getid()
      print *, myb2%getid()

      b1(2)%id = 5
      b2(1)%id = 5

      print *, myb1%getid()
      print *, myb2%getid()

   end associate

      print *, b1%getid()
      print *, b2%getid()

end program
