!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ptrAsgn003.f
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
!*  DESCRIPTION                : Testing:  Pointer assignment
!*                               a) Scalar pointer and target
!*                               Left                       Right
!*                               polymorphic abstract type  polymorphic extension type
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

program ptrAsgn003
   use m

   class(base), pointer :: b1, b2
   class(child), pointer :: c1
   class(child), allocatable, target :: c2

   allocate (c2, source = child(5))

   c1 => c2
   b2 => c2
   b1 => c1

   if (.not. associated(b1,b2) ) error stop 1_4

   if (b1%type() .ne. 2) error stop 2_4
   if (b2%type() .ne. 2) error stop 3_4

   nullify(c1,b2)

   b1 => b2

   if (b1%type() .ne. 1) error stop 4_4
   if (b2%type() .ne. 1) error stop 5_4

   b1 => c1

   if (b1%type() .ne. 1) error stop 6_4

end program
