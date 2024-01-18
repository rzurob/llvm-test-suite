!#######################################################################
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
! %POSTCMD: dcomp proceRef003.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic non-rightmost array object with elemental type-bound
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
      procedure, pass :: getid => getbaseid
   end type

   type, extends(base) :: child
      integer :: rid
   contains
      procedure, pass :: getid => getchildid
   end type


contains

   elemental integer function getbaseid(a)
      class(base), intent(in) :: a
      getbaseid = a%id
   end function

   elemental integer function getchildid(a)
      class(child), intent(in) :: a
      getchildid = a%rid
   end function

end module

program proceRef003
   use m

   type(child), dimension(2) :: c1 = (/ child (1,2),child (4,5) /)
   class(child), allocatable, dimension(:) :: c2
   type(child), pointer, dimension(:,:) :: c3

   allocate(  c2(2), source = (/ child (1,3),child (4,6) /) )
   allocate ( c3(2,2), source = reshape (source = (/child (1,2),child (4,5), c2 /), shape=(/2,2/) ) )

   print *,c1%getid()
   print *,c1%base%getid()

   print *,c2%getid()
   print *,c2%base%getid()

   print *,c3%getid()
   print *,c3%base%getid()

end program