! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/extends/extends005.f
! SCCS ID Information
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
! %POSTCMD: dcomp extends005.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure parent component accessibility
!*                                        parent's data component has private accessibility
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
   type, abstract :: base(k1)    ! (4)
      integer, kind        :: k1
      integer(k1), private :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: r
   end type


contains

   subroutine printbase(a)
      class(base(4)), intent(in) :: a
      print *, a%i
   end subroutine

end module


program extends005
   use m

   type(child(4,4)) :: c1
   class(child(4,4)), allocatable :: c2

   call c1%print()
   print *, c1%i
   allocate(c2, source = child(4,4)(5,5.5) )


end program
