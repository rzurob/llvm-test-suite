! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Final Subroutines
!*                               Base non-abstract type contains a final procedure, extension polymorphic abstract type with no final
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
   type :: base
      integer :: i
   contains
      final :: finalbase1, finalbase2
   end type

   type, extends(base), abstract :: child
      integer :: j
   end type

   type, extends(child) :: gen3
      integer :: k
   contains
      final :: finalgen31, finalgen32
   end type

contains

   subroutine finalbase1(a)
      type(base), intent(inout) :: a
      print *,"finalizebasescalar", a%i
      a%i = 0
   end subroutine

   subroutine finalbase2(a)
      type(base), intent(inout) :: a(:)
      print *,"finalizebasearray", a%i
      a%i = 0
   end subroutine

   subroutine finalgen31(a)
      type(gen3), intent(inout) :: a
      print *,"finalizegen3scalar", a%i, a%j, a%k
      a%j = 0
      a%k=0
   end subroutine

   subroutine finalgen32(a)
      type(gen3), intent(inout) :: a(:)
      print *,"finalizegen3array", a%i, a%j, a%k
      a%j = 0
      a%k =0
   end subroutine

end module

program final003
   use m

   class(child), allocatable :: b1
   class(child), allocatable, dimension(:) :: b2

   allocate (b1, source = gen3(5,6,7))
   allocate (b2(2), source = (/gen3(1,2,3),gen3(3,4,5)/))

   deallocate(b1,b2)

end program

