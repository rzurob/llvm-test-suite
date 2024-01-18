!######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp associate005.f
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
!*                                         1) allocate without source expression of associate-name abstract type components (illegal)
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
      class(base), pointer :: ptr =>null()
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

program associate005
   use m

   class(base), pointer :: b1
   allocate (b1, source = child(8) )

   associate (myb1 => b1)
      allocate(myb1%ptr)
   end associate

end program
