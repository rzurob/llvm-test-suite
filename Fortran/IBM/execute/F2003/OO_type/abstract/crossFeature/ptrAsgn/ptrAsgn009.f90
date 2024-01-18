!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ptrAsgn009.f
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
!*                               a) type component to type component
!*                               Left                       Right
!*                               unlimited polymorphic      polymorphic abstract type
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
      class(base), pointer :: ptr => null()
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

program ptrAsgn009
   use m
   class(*), pointer                :: u1
   class(base), pointer             :: b1
   class(base), allocatable, target :: b2
   type(child), target :: c1 = child(6,null())

   allocate( b2, source=child(5,c1) )

   b1 => b2
   u1 => b2

   if (.not. associated(u1,b1) ) error stop 1_4
   if (.not. associated(u1,b2) ) error stop 2_4

   if (.not. associated(b1%ptr, b2%ptr) ) error stop 3_4
   if ((b1%ptr%type() .ne. 2) .or. (b2%ptr%type() .ne. 2) ) error stop 4_4

   select type(u1)
      type is (child)
         if (u1%ptr%type() .ne. 2) error stop 5_4
         if (.not. associated(u1%ptr, b2%ptr) ) error stop 6_4
      class default
         error stop 7_4
   end select

   nullify(b1%ptr)

   u1 => b1

   select type(u1)
      class is (base)
         if (u1%ptr%type() .ne. 1) error stop 8_4
      class default
         error stop 9_4
   end select

end program
