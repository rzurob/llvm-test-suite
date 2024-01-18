! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic non-rightmost array object call type bound procedures
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

   type, abstract :: base
      integer :: id
   contains
      procedure, nopass :: printtype => printbase
   end type

   type, extends(base) :: child
      integer :: rid
   contains
      procedure, nopass :: printtype => printchild
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

contains

   subroutine printchild()
      print *,'child'
   end subroutine

   subroutine printbase()
      print *,'base'
   end subroutine

end module

program proceRef002
   use m

   type(child), dimension(2) :: c1 = (/ child (1,2),child (4,5) /)
   class(child), allocatable, dimension(:) :: c2
   class(child), pointer, dimension(:) :: c3

   allocate( c2(2), source = (/ child (7,8),child (9,10) /) )
   allocate( c3(2), source = (/ child (0,1),child (2,3) /) )

   call c1%base%printtype()
   call c2%base%printtype()
   call c3%base%printtype()

   call c1(1:1)%base%printtype()
   call c2(2:1:-1)%base%printtype()
   call c3(1:2)%base%printtype()

end program